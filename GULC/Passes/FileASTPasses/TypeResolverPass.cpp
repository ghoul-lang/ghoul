#include <AST/Types/UnresolvedType.hpp>
#include <AST/Types/FunctionTemplateTypenameRefType.hpp>
#include <AST/Exprs/ResolvedTypeRefExpr.hpp>
#include <AST/Exprs/LocalVariableDeclExpr.hpp>
#include <AST/Types/BuiltInType.hpp>
#include <AST/Exprs/IntegerLiteralExpr.hpp>
#include "TypeResolverPass.hpp"

using namespace gulc;

void TypeResolverPass::processFile(FileAST &fileAst) {
    for (Decl* decl : fileAst.topLevelDecls()) {
        processDecl(fileAst, decl);
    }
}

bool TypeResolverPass::resolveType(ResolveTypesContext& context, Type*& type) {
    if (type->getTypeKind() == Type::Kind::Unresolved) {
        // TODO: Take 'namespacePath' into consideration. If there is a namespace path we obviously don't have to worry about the template params
        auto unresolvedType = llvm::dyn_cast<UnresolvedType>(type);

        if (BuiltInType::isBuiltInType(unresolvedType->name())) {
            // TODO: Support template overloading. Allow someone to implement `struct int<T> {}` that will be found if there are template arguments
            if (unresolvedType->hasTemplateArguments()) {
                printError("built in types do not support templating!",
                           context.fileAst, unresolvedType->startPosition(), unresolvedType->endPosition());
            }

            Type *oldType = type;
            type = new BuiltInType(oldType->startPosition(), oldType->endPosition(), unresolvedType->name());
            delete oldType;
            return true;
        }

        // We check the function templates first...
        // Function template params can't be templated themselves?
        if (!unresolvedType->hasTemplateArguments() && context.functionTemplateParams) {
            for (TemplateParameterDecl *templateParameterDecl : *context.functionTemplateParams) {
                if (templateParameterDecl->type->getTypeKind() == Type::Kind::TemplateTypename) {
                    if (templateParameterDecl->name() == unresolvedType->name()) {
                        Type *oldType = type;
                        type = new FunctionTemplateTypenameRefType(oldType->startPosition(), oldType->endPosition(),
                                                                   templateParameterDecl->name());
                        delete oldType;
                        return true;
                    }
                }
            }
        }
    }

    return false;
}

void TypeResolverPass::printError(const std::string &message, FileAST &fileAst, TextPosition startPosition, TextPosition endPosition) {
    std::cout << "gulc resolver error[" << fileAst.filePath() << ", "
                                     "{" << startPosition.line << ", " << startPosition.column << "} "
                                     "to {" << endPosition.line << ", " << endPosition.column << "}]: "
              << message
              << std::endl;
    std::exit(1);
}

void TypeResolverPass::printDebugWarning(const std::string &message, FileAST &fileAst) {
#ifndef NDEBUG
    std::cout << "gulc qualify type pass [DEBUG WARNING](" << fileAst.filePath() << "): " << message << std::endl;
#endif
}

void TypeResolverPass::processDecl(FileAST &fileAst, Decl *decl) {
    switch (decl->getDeclKind()) {
        case Decl::Kind::Function:
            processFunctionDecl(fileAst, llvm::dyn_cast<FunctionDecl>(decl));
            break;
        case Decl::Kind::Parameter:
        case Decl::Kind::TemplateParameterDecl:
        default:
            printDebugWarning("unhandled Decl in 'processDecl'!", fileAst);
            break;
    }
}

void TypeResolverPass::processStmt(ResolveTypesContext& context, Stmt*& stmt) {
    // TODO: Is this everything that can potentially reference a type?
    switch (stmt->getStmtKind()) {
        case Stmt::Kind::Compound:
            processCompoundStmt(context, llvm::dyn_cast<CompoundStmt>(stmt));
            break;
        case Stmt::Kind::Do:
            processDoStmt(context, llvm::dyn_cast<DoStmt>(stmt));
            break;
        case Stmt::Kind::For:
            processForStmt(context, llvm::dyn_cast<ForStmt>(stmt));
            break;
        case Stmt::Kind::If:
            processIfStmt(context, llvm::dyn_cast<IfStmt>(stmt));
            break;
        case Stmt::Kind::Labeled:
            processLabeledStmt(context, llvm::dyn_cast<LabeledStmt>(stmt));
            break;
        case Stmt::Kind::Switch:
            processSwitchStmt(context, llvm::dyn_cast<SwitchStmt>(stmt));
            break;
        case Stmt::Kind::Try:
            processTryStmt(context, llvm::dyn_cast<TryStmt>(stmt));
            break;
        case Stmt::Kind::While:
            processWhileStmt(context, llvm::dyn_cast<WhileStmt>(stmt));
            break;
        case Stmt::Kind::Case:
            processCaseStmt(context, llvm::dyn_cast<CaseStmt>(stmt));
            break;
        case Stmt::Kind::Return:
            processReturnStmt(context, llvm::dyn_cast<ReturnStmt>(stmt));
            break;
        case Stmt::Kind::Expr: {
            auto expr = llvm::dyn_cast<Expr>(stmt);
            processExpr(context, expr);
            stmt = expr;
        }
        default:
            break;
    }
}

void TypeResolverPass::processExpr(ResolveTypesContext& context, Expr*& expr) {
    switch (expr->getExprKind()) {
        case Expr::Kind::BinaryOperator:
            processBinaryOperatorExpr(context, llvm::dyn_cast<BinaryOperatorExpr>(expr));
            break;
        case Expr::Kind::FunctionCall:
            processFunctionCallExpr(context, llvm::dyn_cast<FunctionCallExpr>(expr));
            break;
        case Expr::Kind::Identifier:
            processIdentifierExpr(context, llvm::dyn_cast<IdentifierExpr>(expr));
            break;
        case Expr::Kind::IndexerCall:
            processIndexerCallExpr(context, llvm::dyn_cast<IndexerCallExpr>(expr));
            break;
        case Expr::Kind::LocalVariableDeclOrPrefixOperatorCallExpr:
            // Casting isn't required for this function. It will handle the casting for us since this is a type we will be completely removing from the AST in this function
            processLocalVariableDeclOrPrefixOperatorCallExpr(context, expr);
            break;
        case Expr::Kind::MemberAccessCall:
            processMemberAccessCallExpr(context, llvm::dyn_cast<MemberAccessCallExpr>(expr));
            break;
        case Expr::Kind::Paren:
            processParenExpr(context, llvm::dyn_cast<ParenExpr>(expr));
            break;
        case Expr::Kind::PostfixOperator:
            processPostfixOperatorExpr(context, llvm::dyn_cast<PostfixOperatorExpr>(expr));
            break;
        case Expr::Kind::PotentialExplicitCast:
            processPotentialExplicitCastExpr(context, llvm::dyn_cast<PotentialExplicitCastExpr>(expr));
            break;
        case Expr::Kind::PrefixOperator:
            processPrefixOperatorExpr(context, llvm::dyn_cast<PrefixOperatorExpr>(expr));
            break;
        case Expr::Kind::Ternary:
            processTernaryExpr(context, llvm::dyn_cast<TernaryExpr>(expr));
            break;
        case Expr::Kind::UnresolvedTypeRef:
            processUnresolvedTypeRefExpr(context, expr);
            break;
        default:
            // We don't do anything by default, we only process expressions that explicitly access a type by name
            // TODO: This needs to change when we support custom literal suffixes
            break;
    }
}

// TODO: This should be removed. ResolvedTypeRefExpr should be able to be call like `std.types.int.max()` where `std.types.int` gets converted to a `ResolvedTypeRefExpr`
void TypeResolverPass::processTypeOrExpr(ResolveTypesContext &context, Expr*& expr) {
    // TODO: Currently we only support `Identifier` types.
    switch (expr->getExprKind()) {
        case Expr::Kind::Identifier: {
            auto identifierExpr = llvm::dyn_cast<IdentifierExpr>(expr);
            // TODO: Should we process the template arguments here?
            std::vector<Expr*> templateArguments = identifierExpr->templateArguments;
            Type* unresolvedType = new UnresolvedType(expr->startPosition(), expr->endPosition(), {}, identifierExpr->name(), templateArguments);

            if (resolveType(context, unresolvedType)) {
                identifierExpr->templateArguments.clear();

                Expr* oldExpr = expr;
                expr = new ResolvedTypeRefExpr(oldExpr->startPosition(), oldExpr->endPosition(), unresolvedType);
                delete oldExpr;
            } else {
                templateArguments.clear();

                delete unresolvedType;

                processExpr(context, expr);
            }
        }
        default:
            processExpr(context, expr);
            break;
    }
}

void TypeResolverPass::processFunctionDecl(FileAST &fileAst, FunctionDecl *functionDecl) {
    ResolveTypesContext context(fileAst);

    if (functionDecl->hasTemplateParameters()) {
        context.functionTemplateParams = &functionDecl->templateParameters;
    }

    // Resolve function return type...
    if (!resolveType(context, functionDecl->resultType)) {
        printError("could not find function return type!", fileAst,
                   functionDecl->resultType->startPosition(), functionDecl->resultType->endPosition());
    }

    // Resolve the template parameter types (we allow `void func<typename T, T value>()`)
    for (TemplateParameterDecl*& templateParameterDecl : functionDecl->templateParameters) {
        if (templateParameterDecl->type->getTypeKind() == Type::Kind::Unresolved) {
            if (!resolveType(context, templateParameterDecl->type)) {
                printError("could not find function template type!", fileAst,
                           templateParameterDecl->startPosition(), templateParameterDecl->endPosition());
            }
        }
    }

    // Resolve function parameters
    for (ParameterDecl*& parameterDecl : functionDecl->parameters) {
        if (parameterDecl->type->getTypeKind() == Type::Kind::Unresolved) {
            if (!resolveType(context, parameterDecl->type)) {
                printError("could not find function parameter type!", fileAst,
                           parameterDecl->startPosition(), parameterDecl->endPosition());
            }
        }
    }

    processCompoundStmt(context, functionDecl->body());
}

void TypeResolverPass::processCompoundStmt(ResolveTypesContext &context, CompoundStmt *compoundStmt) {
    for (Stmt*& stmt : compoundStmt->statements()) {
        processStmt(context, stmt);
    }
}

void TypeResolverPass::processDoStmt(ResolveTypesContext &context, DoStmt *doStmt) {
    processStmt(context, doStmt->loopStmt);
    processExpr(context, doStmt->condition);
}

void TypeResolverPass::processForStmt(ResolveTypesContext &context, ForStmt *forStmt) {
    if (forStmt->preLoop != nullptr) processExpr(context, forStmt->preLoop);
    if (forStmt->condition != nullptr) processExpr(context, forStmt->condition);
    if (forStmt->iterationExpr != nullptr) processExpr(context, forStmt->iterationExpr);

    processStmt(context, forStmt->loopStmt);
}

void TypeResolverPass::processIfStmt(ResolveTypesContext &context, IfStmt *ifStmt) {
    processExpr(context, ifStmt->condition);
    processStmt(context, ifStmt->trueStmt);
    if (ifStmt->hasFalseStmt()) processStmt(context, ifStmt->falseStmt);
}

void TypeResolverPass::processLabeledStmt(ResolveTypesContext &context, LabeledStmt *labeledStmt) {
    processStmt(context, labeledStmt->labeledStmt);
}

void TypeResolverPass::processSwitchStmt(ResolveTypesContext &context, SwitchStmt *switchStmt) {
    processExpr(context, switchStmt->condition);

    for (CaseStmt* caseStmt : switchStmt->cases()) {
        processCaseStmt(context, caseStmt);
    }
}

void TypeResolverPass::processTryStmt(ResolveTypesContext &context, TryStmt *tryStmt) {
    processCompoundStmt(context, tryStmt->encapsulatedStmt);

    if (tryStmt->hasCatchStmts()) {
        for (TryCatchStmt*& catchStmt : tryStmt->catchStmts()) {
            processTryCatchStmt(context, catchStmt);
        }
    }

    if (tryStmt->hasFinallyStmt()) {
        processTryFinallyStmt(context, tryStmt->finallyStmt);
    }
}

void TypeResolverPass::processTryCatchStmt(ResolveTypesContext &context, TryCatchStmt *tryCatchStmt) {
    if (tryCatchStmt->hasExceptionDecl()) {
        processExpr(context, tryCatchStmt->exceptionType);
    }

    processCompoundStmt(context, tryCatchStmt->handlerStmt);
}

void TypeResolverPass::processTryFinallyStmt(ResolveTypesContext &context, TryFinallyStmt *tryFinallyStmt) {
    processCompoundStmt(context, tryFinallyStmt->handlerStmt);
}

void TypeResolverPass::processWhileStmt(ResolveTypesContext &context, WhileStmt *whileStmt) {
    processExpr(context, whileStmt->condition);
    processStmt(context, whileStmt->loopStmt);
}

void TypeResolverPass::processCaseStmt(ResolveTypesContext &context, CaseStmt *caseStmt) {
    if (caseStmt->hasCondition()) {
        processExpr(context, caseStmt->condition);
    }

    processStmt(context, caseStmt->trueStmt);
}

void TypeResolverPass::processReturnStmt(ResolveTypesContext &context, ReturnStmt *returnStmt) {
    if (returnStmt->hasReturnValue()) {
        processExpr(context, returnStmt->returnValue);
    }
}

// Expr
void TypeResolverPass::processBinaryOperatorExpr(ResolveTypesContext &context, BinaryOperatorExpr *binaryOperatorExpr) {
    processExpr(context, binaryOperatorExpr->leftValue);
    processExpr(context, binaryOperatorExpr->rightValue);
}

void TypeResolverPass::processFunctionCallExpr(ResolveTypesContext &context, FunctionCallExpr *functionCallExpr) {
    processExpr(context, functionCallExpr->functionReference);

    if (functionCallExpr->hasArguments()) {
        for (Expr*& argument : functionCallExpr->arguments) {
            processExpr(context, argument);
        }
    }
}

void TypeResolverPass::processIdentifierExpr(ResolveTypesContext &context, IdentifierExpr *identifierExpr) {
    if (identifierExpr->hasTemplateArguments()) {
        for (Expr*& templateArgument : identifierExpr->templateArguments) {
            processTypeOrExpr(context, templateArgument);
        }
    }
}

void TypeResolverPass::processIndexerCallExpr(ResolveTypesContext &context, IndexerCallExpr *indexerCallExpr) {
    processTypeOrExpr(context, indexerCallExpr->indexerReference);

    for (Expr*& argument : indexerCallExpr->arguments()) {
        processExpr(context, argument);
    }
}

void TypeResolverPass::processLocalVariableDeclOrPrefixOperatorCallExpr(ResolveTypesContext &context, Expr*& expr) {
    auto localVariableDeclOrPrefixOperatorCallExpr = llvm::dyn_cast<LocalVariableDeclOrPrefixOperatorCallExpr>(expr);

    // TODO: Currently we only support `Identifier` types.
    if (localVariableDeclOrPrefixOperatorCallExpr->nameOrExpr->getExprKind() == Expr::Kind::Identifier) {
        processTypeOrExpr(context, localVariableDeclOrPrefixOperatorCallExpr->typeOrPrefixOperator);

        // If the type of 'typeOrPrefixOperator is now 'ResolvedTypeRef' then that means this is a local variable. If not then it is a prefix operator OR the type just isn't in scope.
        if (localVariableDeclOrPrefixOperatorCallExpr->typeOrPrefixOperator->getExprKind() == Expr::Kind::ResolvedTypeRef) {
            Expr* type = localVariableDeclOrPrefixOperatorCallExpr->typeOrPrefixOperator;

            if (localVariableDeclOrPrefixOperatorCallExpr->nameOrExpr->getExprKind() != Expr::Kind::Identifier) {
                printError("unknown expression in local variable declaration, expected variable name!", context.fileAst,
                           localVariableDeclOrPrefixOperatorCallExpr->nameOrExpr->startPosition(),
                           localVariableDeclOrPrefixOperatorCallExpr->nameOrExpr->endPosition());
                return;
            }

            auto identifier = llvm::dyn_cast<IdentifierExpr>(localVariableDeclOrPrefixOperatorCallExpr->nameOrExpr);

            // Local variable names cannot have template arguments in the name
            if (identifier->hasTemplateArguments()) {
                printError("unknown expression in local variable declaration, expected variable name!", context.fileAst,
                           localVariableDeclOrPrefixOperatorCallExpr->nameOrExpr->startPosition(),
                           localVariableDeclOrPrefixOperatorCallExpr->nameOrExpr->endPosition());
                return;
            }

            std::string variableName = identifier->name();

            expr = new LocalVariableDeclExpr(localVariableDeclOrPrefixOperatorCallExpr->startPosition(),
                                             localVariableDeclOrPrefixOperatorCallExpr->endPosition(),
                                             type, variableName);

            // Set the 'typeOrPrefixOperator' to null since we are still using what it points to...
            localVariableDeclOrPrefixOperatorCallExpr->typeOrPrefixOperator = nullptr;
            // Then delete the old expression since we not longer use it
            delete localVariableDeclOrPrefixOperatorCallExpr;
            return;
        }
    }

    // If we reach this point we can only assume it is a prefix operator call
    if (llvm::isa<IdentifierExpr>(localVariableDeclOrPrefixOperatorCallExpr->typeOrPrefixOperator)) {
        auto identifier = llvm::dyn_cast<IdentifierExpr>(localVariableDeclOrPrefixOperatorCallExpr->typeOrPrefixOperator);
        std::string operatorName = identifier->name();
        Expr* argument = localVariableDeclOrPrefixOperatorCallExpr->nameOrExpr;
        localVariableDeclOrPrefixOperatorCallExpr->nameOrExpr = nullptr;

        processTypeOrExpr(context, argument);

        expr = new PrefixOperatorExpr(localVariableDeclOrPrefixOperatorCallExpr->startPosition(),
                                      localVariableDeclOrPrefixOperatorCallExpr->endPosition(),
                                      operatorName, argument);

        delete localVariableDeclOrPrefixOperatorCallExpr;
        return;
    } else {
        printError("unknown expression where local variable declaration or prefix operator call was expected!", context.fileAst,
                   localVariableDeclOrPrefixOperatorCallExpr->typeOrPrefixOperator->startPosition(),
                   localVariableDeclOrPrefixOperatorCallExpr->typeOrPrefixOperator->endPosition());
        return;
    }
}

void TypeResolverPass::processMemberAccessCallExpr(ResolveTypesContext &context, MemberAccessCallExpr *memberAccessCallExpr) {
    // TODO: MemberAccessCallExpr can ALSO be a namespace path to a type. We will need to take this into account at some point.
    processExpr(context, memberAccessCallExpr->objectRef);
    processIdentifierExpr(context, memberAccessCallExpr->member);
}

void TypeResolverPass::processParenExpr(ResolveTypesContext &context, ParenExpr *parenExpr) {
    processExpr(context, parenExpr->containedExpr);
}

void TypeResolverPass::processPostfixOperatorExpr(ResolveTypesContext &context, PostfixOperatorExpr *postfixOperatorExpr) {
    processExpr(context, postfixOperatorExpr->expr);
}

void TypeResolverPass::processPotentialExplicitCastExpr(ResolveTypesContext &context, PotentialExplicitCastExpr *potentialExplicitCastExpr) {
    processTypeOrExpr(context, potentialExplicitCastExpr->castType);
    processExpr(context, potentialExplicitCastExpr->castee);
}

void TypeResolverPass::processPrefixOperatorExpr(ResolveTypesContext &context, PrefixOperatorExpr *prefixOperatorExpr) {
    processExpr(context, prefixOperatorExpr->expr);
}

void TypeResolverPass::processTernaryExpr(ResolveTypesContext &context, TernaryExpr *ternaryExpr) {
    processExpr(context, ternaryExpr->condition);
    processExpr(context, ternaryExpr->trueExpr);
    processExpr(context, ternaryExpr->falseExpr);
}

void TypeResolverPass::processUnresolvedTypeRefExpr(ResolveTypesContext &context, Expr*& expr) {
    auto unresolvedTypeRefExpr = llvm::dyn_cast<UnresolvedTypeRefExpr>(expr);
    Type* resolvedType = unresolvedTypeRefExpr->unresolvedType;

    if (!resolveType(context, resolvedType)) {
        // TODO: Better error message
        printError("could not resolve type!",
                   context.fileAst, resolvedType->startPosition(), resolvedType->endPosition());
    }

    unresolvedTypeRefExpr->unresolvedType = nullptr;

    expr = new ResolvedTypeRefExpr(unresolvedTypeRefExpr->startPosition(), unresolvedTypeRefExpr->endPosition(), resolvedType);
    delete unresolvedTypeRefExpr;
}
