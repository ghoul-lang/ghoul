#include <AST/Exprs/BinaryOperatorExpr.hpp>
#include <AST/Types/PointerType.hpp>
#include <AST/Types/BuiltInType.hpp>
#include "CodeVerifier.hpp"

using namespace gulc;

void CodeVerifier::verifyFile(FileAST& fileAst) {
    currentFileAst = &fileAst;

    for (Decl* decl : fileAst.topLevelDecls()) {
        verifyDecl(decl);
    }
}

bool CodeVerifier::canCastType(Type* to, Type* from, bool isExplicit) {
    if (llvm::isa<PointerType>(to)) {
        auto toPtr = llvm::dyn_cast<PointerType>(to);

        if (llvm::isa<PointerType>(from)) {
            auto fromPtr = llvm::dyn_cast<PointerType>(from);

            return canCastType(toPtr->pointToType, fromPtr->pointToType, isExplicit);
        } else if (llvm::isa<BuiltInType>(from)) {
            auto fromBuiltIn = llvm::dyn_cast<BuiltInType>(from);

            // We can only cast from a built in type to a pointer type if the built in type is NOT floating...
            return !fromBuiltIn->isFloating();
        } else {
            return false;
        }
    } else if (llvm::isa<BuiltInType>(to)) {
        auto toBuiltIn = llvm::dyn_cast<BuiltInType>(to);

        if (llvm::isa<BuiltInType>(from)) {
            auto fromBuiltIn = llvm::dyn_cast<BuiltInType>(from);

            if (fromBuiltIn->isFloating()) {
                if (toBuiltIn->isFloating()) {
                    if (fromBuiltIn->size() > toBuiltIn->size()) {
                        if (!isExplicit) {
                            // TODO: Start and end positions should be passed to us...
                            printWarning("casting from `" + from->getString() + "` to `" + to->getString() + "` can potentially cause loss of precision!",
                                         toBuiltIn->startPosition(), toBuiltIn->endPosition());
                        }
                    }
                } else {
                    if (!isExplicit) {
                        printWarning("casting from `" + from->getString() + "` to `" + to->getString() + "` will truncate the decimal data!",
                                     toBuiltIn->startPosition(), toBuiltIn->endPosition());
                    }
                }

                return true;
            } else if (fromBuiltIn->isSigned()) {
                if (!toBuiltIn->isSigned()) {
                    if (!isExplicit) {
                        printWarning("casting from signed type `" + from->getString() + "` to unsigned type `" + to->getString() + "` may cause unexpected behaviour!",
                                     toBuiltIn->startPosition(), toBuiltIn->endPosition());
                    }
                }

                return true;
            } else {
                if (toBuiltIn->isSigned()) {
                    if (!isExplicit && fromBuiltIn->size() >= toBuiltIn->size()) {
                        printWarning("casting from unsigned type `" + from->getString() + "` to signed type `" + to->getString() + "` may cause unexpected behaviour!",
                                     toBuiltIn->startPosition(), toBuiltIn->endPosition());
                    }
                }

                return true;
            }
        } else if (llvm::isa<PointerType>(from)) {
            auto fromPtr = llvm::dyn_cast<PointerType>(from);

            if (!isExplicit) {
                // Implicitly casting from pointer to built in is NOT supported
                return false;
            }

            // Cannot cast from pointer to floating or to signed (this is to prevent potential issues with how signed numbers are treated)
            // Though, nothing is stopping someone from casting like: `(double)(ulong)&ptr`
            return !(toBuiltIn->isFloating() || toBuiltIn->isSigned());
        }
    }

    return false;
}

void CodeVerifier::printError(const std::string &message, TextPosition startPosition, TextPosition endPosition) {
    std::cout << "gulc verifier error[" << currentFileAst->filePath() << ", "
                                     "{" << startPosition.line << ", " << startPosition.column << "} "
                                     "to {" << endPosition.line << ", " << endPosition.column << "}]: "
              << message
              << std::endl;
    std::exit(1);
}

void CodeVerifier::printWarning(const std::string &message, TextPosition startPosition, TextPosition endPosition) {
    std::cout << "gulc verifier warning[" << currentFileAst->filePath() << ", "
                                       "{" << startPosition.line << ", " << startPosition.column << "} "
                                       "to {" << endPosition.line << ", " << endPosition.column << "}]: "
              << message
              << std::endl;
}

void CodeVerifier::printDebugWarning(const std::string &message) {
#ifndef NDEBUG
    std::cout << "gulc verifier [DEBUG WARNING](" << currentFileAst->filePath() << "): " << message << std::endl;
#endif
}

void CodeVerifier::verifyDecl(Decl *decl) {
    switch (decl->getDeclKind()) {
        case Decl::Kind::Function:
            verifyFunctionDecl(llvm::dyn_cast<FunctionDecl>(decl));
            break;
        default:
            printDebugWarning("unhandled Decl in 'processDecl'!");
            break;
    }
}

void CodeVerifier::verifyStmt(Stmt*& stmt) {
    switch (stmt->getStmtKind()) {
        case Stmt::Kind::Break:
            verifyBreakStmt(llvm::dyn_cast<BreakStmt>(stmt));
            break;
        case Stmt::Kind::Case:
            verifyCaseStmt(llvm::dyn_cast<CaseStmt>(stmt));
            break;
        case Stmt::Kind::Compound:
            verifyCompoundStmt(llvm::dyn_cast<CompoundStmt>(stmt));
            break;
        case Stmt::Kind::Continue:
            verifyContinueStmt(llvm::dyn_cast<ContinueStmt>(stmt));
            break;
        case Stmt::Kind::Do:
            verifyDoStmt(llvm::dyn_cast<DoStmt>(stmt));
            break;
        case Stmt::Kind::For:
            verifyForStmt(llvm::dyn_cast<ForStmt>(stmt));
            break;
        case Stmt::Kind::Goto:
            verifyGotoStmt(llvm::dyn_cast<GotoStmt>(stmt));
            break;
        case Stmt::Kind::If:
            verifyIfStmt(llvm::dyn_cast<IfStmt>(stmt));
            break;
        case Stmt::Kind::Labeled:
            verifyLabeledStmt(llvm::dyn_cast<LabeledStmt>(stmt));
            break;
        case Stmt::Kind::Return:
            verifyReturnStmt(llvm::dyn_cast<ReturnStmt>(stmt));
            break;
        case Stmt::Kind::Switch:
            verifySwitchStmt(llvm::dyn_cast<SwitchStmt>(stmt));
            break;
        case Stmt::Kind::Try:
            verifyTryStmt(llvm::dyn_cast<TryStmt>(stmt));
            break;
        case Stmt::Kind::TryCatch:
            verifyTryCatchStmt(llvm::dyn_cast<TryCatchStmt>(stmt));
            break;
        case Stmt::Kind::TryFinally:
            verifyTryFinallyStmt(llvm::dyn_cast<TryFinallyStmt>(stmt));
            break;
        case Stmt::Kind::While:
            verifyWhileStmt(llvm::dyn_cast<WhileStmt>(stmt));
            break;
        case Stmt::Kind::Expr: {
            auto expr = llvm::dyn_cast<Expr>(stmt);
            verifyExpr(expr);
            stmt = expr;
        }
    }
}

void CodeVerifier::verifyExpr(Expr*& expr) {
    switch (expr->getExprKind()) {
        case Expr::Kind::BinaryOperator:
            verifyBinaryOperatorExpr(expr);
            break;
        case Expr::Kind::CharacterLiteral:
            verifyCharacterLiteralExpr(llvm::dyn_cast<CharacterLiteralExpr>(expr));
            break;
        case Expr::Kind::ExplicitCast:
            verifyExplicitCastExpr(llvm::dyn_cast<ExplicitCastExpr>(expr));
            break;
        case Expr::Kind::FloatLiteral:
            verifyFloatLiteralExpr(llvm::dyn_cast<FloatLiteralExpr>(expr));
            break;
        case Expr::Kind::FunctionCall:
            verifyFunctionCallExpr(llvm::dyn_cast<FunctionCallExpr>(expr));
            break;
        case Expr::Kind::Identifier:
            verifyIdentifierExpr(expr);
            break;
        case Expr::Kind::ImplicitCast:
            verifyImplicitCastExpr(llvm::dyn_cast<ImplicitCastExpr>(expr));
            break;
        case Expr::Kind::IndexerCall:
            verifyIndexerCallExpr(llvm::dyn_cast<IndexerCallExpr>(expr));
            break;
        case Expr::Kind::IntegerLiteral:
            verifyIntegerLiteralExpr(llvm::dyn_cast<IntegerLiteralExpr>(expr));
            break;
        case Expr::Kind::LocalVariableDecl:
            verifyLocalVariableDeclExpr(llvm::dyn_cast<LocalVariableDeclExpr>(expr));
            break;
        case Expr::Kind::LocalVariableDeclOrPrefixOperatorCallExpr:
            // Casting isn't required for this function. It will handle the casting for us since this is a type we will be completely removing from the AST in this function
            verifyLocalVariableDeclOrPrefixOperatorCallExpr(expr);
            break;
        case Expr::Kind::LValueToRValue:
            verifyLValueToRValueExpr(llvm::dyn_cast<LValueToRValueExpr>(expr));
            break;
        case Expr::Kind::MemberAccessCall:
            verifyMemberAccessCallExpr(llvm::dyn_cast<MemberAccessCallExpr>(expr));
            break;
        case Expr::Kind::Paren:
            verifyParenExpr(llvm::dyn_cast<ParenExpr>(expr));
            break;
        case Expr::Kind::PostfixOperator:
            verifyPostfixOperatorExpr(llvm::dyn_cast<PostfixOperatorExpr>(expr));
            break;
        case Expr::Kind::PotentialExplicitCast:
            verifyPotentialExplicitCastExpr(llvm::dyn_cast<PotentialExplicitCastExpr>(expr));
            break;
        case Expr::Kind::PrefixOperator:
            verifyPrefixOperatorExpr(llvm::dyn_cast<PrefixOperatorExpr>(expr));
            break;
        case Expr::Kind::ResolvedTypeRef:
            verifyResolvedTypeRefExpr(llvm::dyn_cast<ResolvedTypeRefExpr>(expr));
            break;
        case Expr::Kind::StringLiteral:
            verifyStringLiteralExpr(llvm::dyn_cast<StringLiteralExpr>(expr));
            break;
        case Expr::Kind::Ternary:
            verifyTernaryExpr(llvm::dyn_cast<TernaryExpr>(expr));
            break;
        case Expr::Kind::UnresolvedTypeRef:
            verifyUnresolvedTypeRefExpr(expr);
            break;
    }
}

// Decls
void CodeVerifier::verifyFunctionDecl(FunctionDecl *functionDecl) {
    currentFunctionReturnType = functionDecl->resultType;
    currentFunctionTemplateParameters = &functionDecl->templateParameters;
    currentFunctionParameters = &functionDecl->parameters;

    currentFunctionLocalVariablesCount = 0;

    // TODO: Make sure the function name isn't already taken
    verifyCompoundStmt(functionDecl->body());

    currentFunctionLocalVariablesCount = 0;

    currentFunctionReturnType = nullptr;
    currentFunctionTemplateParameters = nullptr;
    currentFunctionParameters = nullptr;
}

// Stmts
void CodeVerifier::verifyBreakStmt(BreakStmt *breakStmt) {
    // TODO: I don't think we need to do anything here. The labels have already been verified in `DeclResolver`?
}

void CodeVerifier::verifyCaseStmt(CaseStmt *caseStmt) {
    // TODO: Should we verify that the condition result type is the same as the switch value type?
    verifyExpr(caseStmt->condition);
    verifyStmt(caseStmt->trueStmt);
}

void CodeVerifier::verifyCompoundStmt(CompoundStmt *compoundStmt) {
    currentFunctionLocalVariablesCount = 0;

    for (Stmt* stmt : compoundStmt->statements()) {
        verifyStmt(stmt);
    }

    currentFunctionLocalVariablesCount = 0;
}

void CodeVerifier::verifyContinueStmt(ContinueStmt *continueStmt) {
    // TODO: I don't think we need to do anything here. The labels have already been verified in `DeclResolver`?
}

void CodeVerifier::verifyDoStmt(DoStmt *doStmt) {
    verifyStmt(doStmt->loopStmt);
    // TODO: Should we verify the condition result is a boolean?
    verifyExpr(doStmt->condition);
}

void CodeVerifier::verifyForStmt(ForStmt *forStmt) {
    verifyExpr(forStmt->preLoop);
    verifyExpr(forStmt->condition);

    verifyStmt(forStmt->loopStmt);

    verifyExpr(forStmt->iterationExpr);
}

void CodeVerifier::verifyGotoStmt(GotoStmt *gotoStmt) {
    // TODO: Nothing to do, The label should already have been checked in the `DeclResolver`?
}

void CodeVerifier::verifyIfStmt(IfStmt *ifStmt) {
    // TODO: We should support stuff like `if (Widget^ w = new Button("Test")) {}`
    verifyExpr(ifStmt->condition);

    verifyStmt(ifStmt->trueStmt);

    if (ifStmt->hasFalseStmt()) {
        verifyStmt(ifStmt->falseStmt);
    }
}

void CodeVerifier::verifyLabeledStmt(LabeledStmt *labeledStmt) {
    // The label has already been checked for ambiguity in `DeclResovler`
    verifyStmt(labeledStmt->labeledStmt);
}

void CodeVerifier::verifyReturnStmt(ReturnStmt *returnStmt) {
    // TODO: Should we verify the return value result type is the same as the result type of the current function?
    verifyExpr(returnStmt->returnValue);
}

void CodeVerifier::verifySwitchStmt(SwitchStmt *switchStmt) {
    verifyExpr(switchStmt->condition);

    for (CaseStmt*& caseStmt : switchStmt->cases()) {
        verifyCaseStmt(caseStmt);
    }
}

void CodeVerifier::verifyTryStmt(TryStmt *tryStmt) {
    verifyCompoundStmt(tryStmt->encapsulatedStmt);

    if (tryStmt->hasCatchStmts()) {
        for (TryCatchStmt *catchStmt : tryStmt->catchStmts()) {
            verifyTryCatchStmt(catchStmt);
        }
    }

    if (tryStmt->hasFinallyStmt()) {
        verifyTryFinallyStmt(tryStmt->finallyStmt);
    }
}

void CodeVerifier::verifyTryCatchStmt(TryCatchStmt *tryCatchStmt) {
    verifyCompoundStmt(tryCatchStmt->handlerStmt);
}

void CodeVerifier::verifyTryFinallyStmt(TryFinallyStmt *tryFinallyStmt) {
    verifyCompoundStmt(tryFinallyStmt->handlerStmt);
}

void CodeVerifier::verifyWhileStmt(WhileStmt *whileStmt) {
    verifyExpr(whileStmt->condition);
    verifyStmt(whileStmt->loopStmt);
}

// Exprs
void CodeVerifier::verifyBinaryOperatorExpr(Expr *&expr) {
    auto binaryOperatorExpr = llvm::dyn_cast<BinaryOperatorExpr>(expr);

    verifyExpr(binaryOperatorExpr->leftValue);
    verifyExpr(binaryOperatorExpr->rightValue);

    if (binaryOperatorExpr->isBuiltInAssignmentOperator()) {
        // TODO: Check that the left value is assignable...
    }
}

void CodeVerifier::verifyCharacterLiteralExpr(CharacterLiteralExpr *characterLiteralExpr) {
    // TODO: I don't think there really is anything to verify here?
}

void CodeVerifier::verifyExplicitCastExpr(ExplicitCastExpr *explicitCastExpr) {
    // TODO: Support explicit cast overloading by checking the castee return type for overloaded cast to the castType
    //  (don't check the castType for support for casting from the castee type...)
    verifyExpr(explicitCastExpr->castee);

    if (!canCastType(explicitCastExpr->castType, explicitCastExpr->castee->resultType, true)) {
        printError("cannot cast to type `" + explicitCastExpr->castType->getString() + "` from `" + explicitCastExpr->castee->resultType->getString() + "`!",
                   explicitCastExpr->startPosition(), explicitCastExpr->endPosition());
    }
}

void CodeVerifier::verifyFloatLiteralExpr(FloatLiteralExpr *floatLiteralExpr) {
    // TODO: I don't think there really is anything to verify here?
}

void CodeVerifier::verifyFunctionCallExpr(FunctionCallExpr *functionCallExpr) {
    for (Expr*& argument : functionCallExpr->arguments) {
        verifyExpr(argument);
    }
}

void CodeVerifier::verifyIdentifierExpr(Expr *&identifierExpr) {
    // TODO: We should verify the template parameters when we support them...
}

void CodeVerifier::verifyImplicitCastExpr(ImplicitCastExpr *implicitCastExpr) {
    // TODO: Don't support implicitly casting from int to pointer when it is an assignment from an rvalue?
    // TODO: We should support overloading by checking the castee return type
    verifyExpr(implicitCastExpr->castee);

    // TODO: There are some casts we might want to warn about when implicitly casting
    if (!canCastType(implicitCastExpr->castType, implicitCastExpr->castee->resultType, false)) {
        printError("cannot implicitly cast to type `" + implicitCastExpr->castType->getString() + "` from `" + implicitCastExpr->castee->resultType->getString() + "`!",
                   implicitCastExpr->startPosition(), implicitCastExpr->endPosition());
    }
}

void CodeVerifier::verifyIndexerCallExpr(IndexerCallExpr *indexerCallExpr) {
    printError("indexing not yet supported!",
               indexerCallExpr->startPosition(), indexerCallExpr->endPosition());
}

void CodeVerifier::verifyIntegerLiteralExpr(IntegerLiteralExpr *integerLiteralExpr) {
    // TODO: I don't think there really is anything to verify here?
}

void CodeVerifier::verifyLocalVariableDeclExpr(LocalVariableDeclExpr *localVariableDeclExpr) {
    // TODO: I don't think there really is anything to verify here?
}

void CodeVerifier::verifyLocalVariableDeclOrPrefixOperatorCallExpr(Expr *&expr) {
    printError("[INTERNAL] found `LocalVariableDeclOrPrefixOperatorCallExpr` in verifier, this is not supported!",
               expr->startPosition(), expr->endPosition());
}

void CodeVerifier::verifyLValueToRValueExpr(LValueToRValueExpr *expr) {
    verifyExpr(expr->lvalue);
}

void CodeVerifier::verifyMemberAccessCallExpr(MemberAccessCallExpr *memberAccessCallExpr) {
    printError("member access calls not yet supported!",
               memberAccessCallExpr->startPosition(), memberAccessCallExpr->endPosition());
}

void CodeVerifier::verifyParenExpr(ParenExpr *parenExpr) {
    verifyExpr(parenExpr->containedExpr);
}

void CodeVerifier::verifyPostfixOperatorExpr(PostfixOperatorExpr *postfixOperatorExpr) {
    verifyExpr(postfixOperatorExpr->expr);
}

void CodeVerifier::verifyPotentialExplicitCastExpr(PotentialExplicitCastExpr *potentialExplicitCastExpr) {
    printError("[INTERNAL] found `PotentialExplicitCastExpr` in verifier, this is not supported!",
               potentialExplicitCastExpr->startPosition(), potentialExplicitCastExpr->endPosition());
}

void CodeVerifier::verifyPrefixOperatorExpr(PrefixOperatorExpr *prefixOperatorExpr) {
    verifyExpr(prefixOperatorExpr->expr);
}

void CodeVerifier::verifyResolvedTypeRefExpr(ResolvedTypeRefExpr *resolvedTypeRefExpr) {
    // TODO: I don't think there really is anything to verify here?
}

void CodeVerifier::verifyStringLiteralExpr(StringLiteralExpr *stringLiteralExpr) {
    // TODO: I don't think there really is anything to verify here?
}

void CodeVerifier::verifyTernaryExpr(TernaryExpr *ternaryExpr) {
    // TODO: We should verify the condition is a boolean
    verifyExpr(ternaryExpr->condition);
    verifyExpr(ternaryExpr->trueExpr);
    verifyExpr(ternaryExpr->falseExpr);
}

void CodeVerifier::verifyUnresolvedTypeRefExpr(Expr *&expr) {
    printError("[INTERNAL] found `UnresolvedTypeRefExpr` in verifier, this is not supported!",
               expr->startPosition(), expr->endPosition());
}
