#include <AST/Exprs/BinaryOperatorExpr.hpp>
#include <AST/Decls/StructDecl.hpp>
#include <AST/Types/StructType.hpp>
#include <AST/Exprs/RefLocalVariableExpr.hpp>
#include <AST/Types/ConstType.hpp>
#include <AST/Types/MutType.hpp>
#include <AST/Types/ImmutType.hpp>
#include <AST/Types/ReferenceType.hpp>
#include "Lifetimes.hpp"

using namespace gulc;

void Lifetimes::processFile(std::vector<FileAST *> &files) {
    for (FileAST* fileAst : files) {
        currentFileAst = fileAst;

        for (Decl *decl : fileAst->topLevelDecls()) {
            processDecl(decl);
        }
    }
}

void Lifetimes::printError(const std::string &message, TextPosition startPosition, TextPosition endPosition) {
    std::cout << "gulc codegen error[" << currentFileAst->filePath() << ", "
                                    "{" << startPosition.line << ", " << startPosition.column << "} "
                                    "to {" << endPosition.line << ", " << endPosition.column << "}]: "
              << message
              << std::endl;
    std::exit(1);
}

void Lifetimes::processDecl(Decl *decl) {
    switch (decl->getDeclKind()) {
        case Decl::Kind::Function:
            processFunctionDecl(llvm::dyn_cast<FunctionDecl>(decl));
            break;
        case Decl::Kind::Namespace:
            processNamespaceDecl(llvm::dyn_cast<NamespaceDecl>(decl));
            break;
        case Decl::Kind::Struct:
            processStructDecl(llvm::dyn_cast<StructDecl>(decl));
            break;
        case Decl::Kind::TemplateFunction:
            processTemplateFunctionDecl(llvm::dyn_cast<TemplateFunctionDecl>(decl));
            break;
        default:
            break;
    }
}

void Lifetimes::processStmt(Stmt *&stmt) {
    switch (stmt->getStmtKind()) {
        case Stmt::Kind::Break:
            processBreakStmt(llvm::dyn_cast<BreakStmt>(stmt));
            break;
        case Stmt::Kind::Case:
            processCaseStmt(llvm::dyn_cast<CaseStmt>(stmt));
            break;
        case Stmt::Kind::Compound:
            processCompoundStmt(llvm::dyn_cast<CompoundStmt>(stmt), false);
            break;
        case Stmt::Kind::Continue:
            processContinueStmt(llvm::dyn_cast<ContinueStmt>(stmt));
            break;
        case Stmt::Kind::Do:
            processDoStmt(llvm::dyn_cast<DoStmt>(stmt));
            break;
        case Stmt::Kind::For:
            processForStmt(llvm::dyn_cast<ForStmt>(stmt));
            break;
        case Stmt::Kind::Goto:
            processGotoStmt(llvm::dyn_cast<GotoStmt>(stmt));
            break;
        case Stmt::Kind::If:
            processIfStmt(llvm::dyn_cast<IfStmt>(stmt));
            break;
        case Stmt::Kind::Labeled:
            processLabeledStmt(llvm::dyn_cast<LabeledStmt>(stmt));
            break;
        case Stmt::Kind::Return:
            processReturnStmt(llvm::dyn_cast<ReturnStmt>(stmt));
            break;
        case Stmt::Kind::Switch:
            processSwitchStmt(llvm::dyn_cast<SwitchStmt>(stmt));
            break;
        case Stmt::Kind::Try:
            processTryStmt(llvm::dyn_cast<TryStmt>(stmt));
            break;
        case Stmt::Kind::While:
            processWhileStmt(llvm::dyn_cast<WhileStmt>(stmt));
            break;
        case Stmt::Kind::Expr: {
            Expr *tmpExpr = llvm::dyn_cast<Expr>(stmt);
            processExpr(tmpExpr);
            stmt = tmpExpr;
            break;
        }
        default:
            break;
    }
}

void Lifetimes::processExpr(Expr *&expr) {
    switch (expr->getExprKind()) {
        case Expr::Kind::BinaryOperator:
            processBinaryOperatorExpr(expr);
            break;
        case Expr::Kind::CharacterLiteral:
            processCharacterLiteralExpr(llvm::dyn_cast<CharacterLiteralExpr>(expr));
            break;
        case Expr::Kind::ExplicitCast:
            processExplicitCastExpr(llvm::dyn_cast<ExplicitCastExpr>(expr));
            break;
        case Expr::Kind::FloatLiteral:
            processFloatLiteralExpr(llvm::dyn_cast<FloatLiteralExpr>(expr));
            break;
        case Expr::Kind::FunctionCall:
            processFunctionCallExpr(llvm::dyn_cast<FunctionCallExpr>(expr));
            break;
        case Expr::Kind::Identifier:
            // TODO: I don't think we need to handle these? Shouldn't these be errors at this point?
            processIdentifierExpr(expr);
            break;
        case Expr::Kind::ImplicitCast:
            processImplicitCastExpr(llvm::dyn_cast<ImplicitCastExpr>(expr));
            break;
        case Expr::Kind::IndexerCall:
            processIndexerCallExpr(llvm::dyn_cast<IndexerCallExpr>(expr));
            break;
        case Expr::Kind::IntegerLiteral:
            processIntegerLiteralExpr(llvm::dyn_cast<IntegerLiteralExpr>(expr));
            break;
        case Expr::Kind::LocalVariableDecl:
            processLocalVariableDeclExpr(llvm::dyn_cast<LocalVariableDeclExpr>(expr));
            break;
        case Expr::Kind::MemberAccessCall:
            processMemberAccessCallExpr(llvm::dyn_cast<MemberAccessCallExpr>(expr));
            break;
        case Expr::Kind::Paren:
            processParenExpr(llvm::dyn_cast<ParenExpr>(expr));
            break;
        case Expr::Kind::PostfixOperator:
            processPostfixOperatorExpr(llvm::dyn_cast<PostfixOperatorExpr>(expr));
            break;
        case Expr::Kind::PrefixOperator:
            processPrefixOperatorExpr(llvm::dyn_cast<PrefixOperatorExpr>(expr));
            break;
        case Expr::Kind::StringLiteral:
            processStringLiteralExpr(llvm::dyn_cast<StringLiteralExpr>(expr));
            break;
        case Expr::Kind::Ternary:
            processTernaryExpr(llvm::dyn_cast<TernaryExpr>(expr));
            break;
        default:
            break;
    }
}

// Decls
void Lifetimes::processConstructorDecl(ConstructorDecl *constructorDecl) {
    functionParams = &constructorDecl->parameters;

    processCompoundStmt(constructorDecl->body(), true);
}

void Lifetimes::processDestructorDecl(DestructorDecl *destructorDecl) {
    currentFunctionIsDestructor = true;

    functionParams = nullptr;

    processCompoundStmt(destructorDecl->body(), true);

    currentFunctionIsDestructor = false;
}

void Lifetimes::processFunctionDecl(FunctionDecl *functionDecl) {
    currentLocalVariablesCount = 0;

    currentFunction = functionDecl;
    functionParams = &functionDecl->parameters;

    processCompoundStmt(functionDecl->body(), true);

    currentFunction = nullptr;
}

void Lifetimes::processNamespaceDecl(NamespaceDecl *namespaceDecl) {
    NamespaceDecl* oldNamespace = currentNamespace;
    currentNamespace = namespaceDecl;

    for (Decl* decl : namespaceDecl->nestedDecls()) {
        processDecl(decl);
    }

    currentNamespace = oldNamespace;
}

void Lifetimes::processStructDecl(StructDecl *structDecl) {
    StructDecl* oldStruct = currentStruct;
    currentStruct = structDecl;

    for (ConstructorDecl* constructor : structDecl->constructors) {
        processConstructorDecl(constructor);
    }

    for (Decl* member : structDecl->members) {
        processDecl(member);
    }

    if (structDecl->destructor != nullptr) {
        processDestructorDecl(structDecl->destructor);
    }

    currentStruct = oldStruct;
}

void Lifetimes::processTemplateFunctionDecl(TemplateFunctionDecl *templateFunctionDecl) {
    for (FunctionDecl* implementedFunction : templateFunctionDecl->implementedFunctions()) {
        processTemplateFunctionDeclImplementation(templateFunctionDecl, implementedFunction);
    }
}

void Lifetimes::processTemplateFunctionDeclImplementation(TemplateFunctionDecl *templateFunctionDecl,
                                                          FunctionDecl *implementedFunction) {
    // TODO: Should we do anything else?
    processFunctionDecl(implementedFunction);
}

// Stmts
void Lifetimes::processBreakStmt(BreakStmt *breakStmt) {
    if (breakStmt->label().empty()) {
        destructLocalVariablesDeclaredAfterLoop(currentLoop, breakStmt->preBreakCleanup);
    } else {
        LabeledStmt* breakLabel = currentFunction->labeledStmts[breakStmt->label()];
        // If the labeled statement isn't a loop that will be handled in the `CodeVerifier` since that isn't allowed.
        destructLocalVariablesDeclaredAfterLoop(breakLabel->labeledStmt, breakStmt->preBreakCleanup);
    }
}

void Lifetimes::processCaseStmt(CaseStmt *caseStmt) {
    if (caseStmt->hasCondition()) {
        processExpr(caseStmt->condition);
    }

    processStmt(caseStmt->trueStmt);
}

void Lifetimes::processCompoundStmt(CompoundStmt *compoundStmt, bool isFunctionBody) {
    unsigned int oldLocalVariableCount = currentLocalVariablesCount;

    for (Stmt*& stmt : compoundStmt->statements()) {
        processStmt(stmt);
    }

    // We only call the destructors if it isn't the function body. If the compound statement is the function body then
    // there will always be a `return` placed at the very end of the function body that will handle destructors
    // If we do this at the end of the function body after a `return` then LLVM will seg fault.
    if (!isFunctionBody) {
        // If we've created new local variables then we have to destruct them
        for (std::int64_t i = static_cast<int64_t>(currentLocalVariablesCount) - 1; i >= oldLocalVariableCount; --i) {
            // Add the destructor call to the end of the compound statement...
            DestructLocalVariableExpr *destructLocalVariableExpr = destructLocalVariable(currentLocalVariables[i]);

            if (destructLocalVariableExpr != nullptr) {
                compoundStmt->statements().push_back(destructLocalVariableExpr);
            }
        }
    }

    currentLocalVariablesCount = oldLocalVariableCount;
}

void Lifetimes::processContinueStmt(ContinueStmt *continueStmt) {
    if (continueStmt->label().empty()) {
        destructLocalVariablesDeclaredAfterLoop(currentLoop, continueStmt->preContinueCleanup);
    } else {
        LabeledStmt* continueLabel = currentFunction->labeledStmts[continueStmt->label()];
        // If the labeled statement isn't a loop that will be handled in the `CodeVerifier` since that isn't allowed.
        destructLocalVariablesDeclaredAfterLoop(continueLabel->labeledStmt, continueStmt->preContinueCleanup);
    }
}

void Lifetimes::processDoStmt(DoStmt *doStmt) {
    auto oldLoop = currentLoop;

    doStmt->currentNumLocalVariables = currentLocalVariablesCount;
    currentLoop = doStmt;

    if (doStmt->loopStmt != nullptr) processStmt(doStmt->loopStmt);
    processExpr(doStmt->condition);

    currentLoop = oldLoop;
}

void Lifetimes::processForStmt(ForStmt *forStmt) {
    auto oldLoop = currentLoop;

    unsigned int oldLocalVariableCount = currentLocalVariablesCount;

    if (forStmt->preLoop != nullptr) processExpr(forStmt->preLoop);
    if (forStmt->condition != nullptr) processExpr(forStmt->condition);
    if (forStmt->iterationExpr != nullptr) processExpr(forStmt->iterationExpr);

    forStmt->currentNumLocalVariables = currentLocalVariablesCount;
    currentLoop = forStmt;

    if (forStmt->loopStmt != nullptr) processStmt(forStmt->loopStmt);

    // If we've created new local variables then we have to destruct them
    for (std::int64_t i = static_cast<int64_t>(currentLocalVariablesCount) - 1; i >= oldLocalVariableCount; --i) {
        // Add the destructor call to the end of the compound statement...
        DestructLocalVariableExpr *destructLocalVariableExpr = destructLocalVariable(currentLocalVariables[i]);

        if (destructLocalVariableExpr != nullptr) {
            forStmt->postLoopCleanup.push_back(destructLocalVariableExpr);
        }
    }

    currentLoop = oldLoop;
    currentLocalVariablesCount = oldLocalVariableCount;
}

void Lifetimes::processGotoStmt(GotoStmt *gotoStmt) {
    LabeledStmt* gotoLabel = currentFunction->labeledStmts[gotoStmt->label];

    // The `CodeVerifier` pass should handle verifying that no new variable have been declared between us and the
    // labeled statement
    for (std::int64_t i = static_cast<int64_t>(currentLocalVariablesCount) - 1; i >= gotoLabel->currentNumLocalVariables; --i) {
        // Add the destructor call to the end of the compound statement...
        DestructLocalVariableExpr *destructLocalVariableExpr = destructLocalVariable(currentLocalVariables[i]);

        if (destructLocalVariableExpr != nullptr) {
            gotoStmt->preGotoCleanup.push_back(destructLocalVariableExpr);
        }
    }
}

void Lifetimes::processIfStmt(IfStmt *ifStmt) {
    processExpr(ifStmt->condition);
    if (ifStmt->trueStmt != nullptr) processStmt(ifStmt->trueStmt);

    if (ifStmt->hasFalseStmt()) {
        processStmt(ifStmt->falseStmt);
    }
}

void Lifetimes::processLabeledStmt(LabeledStmt *labeledStmt) {
    processStmt(labeledStmt->labeledStmt);
}

void Lifetimes::processReturnStmt(ReturnStmt *returnStmt) {
    if (returnStmt->returnValue != nullptr) processExpr(returnStmt->returnValue);

    // Add the destructor calls to the pre return expression list
    for (int64_t i = static_cast<int64_t>(currentLocalVariablesCount) - 1; i >= 0; --i) {
        DestructLocalVariableExpr* destructLocalVariableExpr = destructLocalVariable(currentLocalVariables[i]);

        if (destructLocalVariableExpr != nullptr) {
            returnStmt->preReturnExprs.push_back(destructLocalVariableExpr);
        }
    }

    // We only destruct parameters on return statements
    if (functionParams != nullptr) {
        for (int64_t i = static_cast<int64_t>(functionParams->size()) - 1; i >= 0; --i) {
            DestructParameterExpr *destructParameterExpr = destructParameter(i, (*functionParams)[i]);

            if (destructParameterExpr != nullptr) {
                returnStmt->preReturnExprs.push_back(destructParameterExpr);
            }
        }
    }

    if (currentFunctionIsDestructor) {
        for (int64_t i = static_cast<int64_t>(currentStruct->dataMembers.size()) - 1; i >= 0; --i) {
            GlobalVariableDecl* memberVariable = currentStruct->dataMembers[i];

            Type* checkType = memberVariable->type;

            if (llvm::isa<ConstType>(checkType)) {
                checkType = llvm::dyn_cast<ConstType>(checkType)->pointToType;
            } else if (llvm::isa<MutType>(checkType)) {
                checkType = llvm::dyn_cast<MutType>(checkType)->pointToType;
            } else if (llvm::isa<ImmutType>(checkType)) {
                checkType = llvm::dyn_cast<ImmutType>(checkType)->pointToType;
            }

            if (llvm::isa<StructType>(checkType)) {
                auto structType = llvm::dyn_cast<StructType>(checkType);

                if (structType->decl()->destructor == nullptr) {
                    printError("[INTERNAL] struct `" + structType->decl()->name() + "` is missing a destructor!",
                               memberVariable->startPosition(), memberVariable->endPosition());
                }

                // TODO: Clean this up. A lot of this code already exists in `DeclResolver`...
                StructType *thisType = new StructType({}, {}, currentStruct->name(), currentStruct);

                RefParameterExpr *refThisParam = new RefParameterExpr({}, {}, 0);
                refThisParam->resultType = new ReferenceType({}, {}, thisType->deepCopy());
                // A parameter reference is an lvalue.
                refThisParam->resultType->setIsLValue(true);

                // Dereference the `this` parameter
                auto derefThisParam = new PrefixOperatorExpr(refThisParam->startPosition(),
                                                             refThisParam->endPosition(),
                                                             ".deref", refThisParam);
                derefThisParam->resultType = thisType->deepCopy();
                // A dereferenced value is an lvalue
                derefThisParam->resultType->setIsLValue(true);

                RefStructMemberVariableExpr *refMemberVariable = new RefStructMemberVariableExpr({}, {}, derefThisParam,
                                                                                                 thisType,
                                                                                                 memberVariable);

                DestructMemberVariableExpr *destructMemberVariable =
                        new DestructMemberVariableExpr({}, {}, refMemberVariable, structType->decl()->destructor);

                returnStmt->preReturnExprs.push_back(destructMemberVariable);
            }
        }
    }
}

void Lifetimes::processSwitchStmt(SwitchStmt *switchStmt) {
    processExpr(switchStmt->condition);

    for (CaseStmt* caseStmt : switchStmt->cases()) {
        processCaseStmt(caseStmt);
    }
}

void Lifetimes::processTryStmt(TryStmt *tryStmt) {
    processCompoundStmt(tryStmt->encapsulatedStmt, false);

    for (TryCatchStmt* tryCatchStmt : tryStmt->catchStmts()) {
        processTryCatchStmt(tryCatchStmt);
    }

    if (tryStmt->hasFinallyStmt()) {
        processTryFinallyStmt(tryStmt->finallyStmt);
    }
}

void Lifetimes::processTryCatchStmt(TryCatchStmt *tryCatchStmt) {
    processCompoundStmt(tryCatchStmt->handlerStmt, false);
}

void Lifetimes::processTryFinallyStmt(TryFinallyStmt *tryFinallyStmt) {
    processCompoundStmt(tryFinallyStmt->handlerStmt, false);
}

void Lifetimes::processWhileStmt(WhileStmt *whileStmt) {
    auto oldLoop = currentLoop;

    processExpr(whileStmt->condition);

    whileStmt->currentNumLocalVariables = currentLocalVariablesCount;
    currentLoop = whileStmt;

    if (whileStmt->loopStmt != nullptr) processStmt(whileStmt->loopStmt);

    currentLoop = oldLoop;
}

// Exprs
void Lifetimes::processBinaryOperatorExpr(Expr *&expr) {
    auto binaryOperatorExpr = llvm::dyn_cast<BinaryOperatorExpr>(expr);

    // Replace any assignments with constructor calls if we can...
    if (llvm::isa<LocalVariableDeclExpr>(binaryOperatorExpr->leftValue)) {
        auto localVariableDeclExpr = llvm::dyn_cast<LocalVariableDeclExpr>(binaryOperatorExpr->leftValue);

        if (localVariableDeclExpr->hasInitializer()) {
            printError("use of initializer with an assignment operator `=` is not allowed!",
                       expr->startPosition(), expr->endPosition());
        }

        if (llvm::isa<ResolvedTypeRefExpr>(localVariableDeclExpr->type)) {
            auto localVariableTypeRef = llvm::dyn_cast<ResolvedTypeRefExpr>(localVariableDeclExpr->type);

            if (TypeComparer::getTypesAreSame(localVariableTypeRef->resolvedType,
                                              binaryOperatorExpr->rightValue->resultType)) {
                Type* initializerType = binaryOperatorExpr->rightValue->resultType;
                localVariableDeclExpr->initializerArgs.push_back(binaryOperatorExpr->rightValue);

                // Remove any reference to the local variable declaration and the initial argument
                // Then delete the no longer needed binary operator expression
                binaryOperatorExpr->leftValue = nullptr;
                binaryOperatorExpr->rightValue = nullptr;
                delete binaryOperatorExpr;
                // Make the local variable declaration the new expression where the binary operator once was
                expr = localVariableDeclExpr;

                // If the resolved type of the local variable is a struct then we search for a constructor
                if (llvm::isa<StructType>(localVariableTypeRef->resolvedType)) {
                    auto checkStructType = llvm::dyn_cast<StructType>(localVariableTypeRef->resolvedType);

                    for (ConstructorDecl* checkConstructor : checkStructType->decl()->constructors) {
                        // TODO: Once we support `copy` and `move` constructors this will have to change...
                        if (checkConstructor->parameters.size() == 1) {
                            // If the types are the same we set the local variable's constructor reference and break from the loop
                            if (TypeComparer::getTypesAreSame(checkConstructor->parameters[0]->type, initializerType)) {
                                localVariableDeclExpr->foundConstructor = checkConstructor;
                                break;
                            }
                        }
                    }
                }
            }
        }

        // We still pass the local variable declaration to be processed so we keep track of it for destruction
        processLocalVariableDeclExpr(localVariableDeclExpr);
    }
}

void Lifetimes::processCharacterLiteralExpr(CharacterLiteralExpr *characterLiteralExpr) {

}

void Lifetimes::processExplicitCastExpr(ExplicitCastExpr *explicitCastExpr) {
    processExpr(explicitCastExpr->castee);
}

void Lifetimes::processFloatLiteralExpr(FloatLiteralExpr *floatLiteralExpr) {

}

void Lifetimes::processFunctionCallExpr(FunctionCallExpr *functionCallExpr) {
    for (Expr*& argument : functionCallExpr->arguments) {
        processExpr(argument);
    }

    processExpr(functionCallExpr->functionReference);
}

void Lifetimes::processIdentifierExpr(Expr *&identifierExpr) {
    // TODO: Shouldn't this be an error at this point?
}

void Lifetimes::processImplicitCastExpr(ImplicitCastExpr *implicitCastExpr) {
    processExpr(implicitCastExpr->castee);
}

void Lifetimes::processIndexerCallExpr(IndexerCallExpr *indexerCallExpr) {
    for (Expr*& argument : indexerCallExpr->arguments()) {
        processExpr(argument);
    }

    processExpr(indexerCallExpr->indexerReference);
}

void Lifetimes::processIntegerLiteralExpr(IntegerLiteralExpr *integerLiteralExpr) {

}

void Lifetimes::processLocalVariableDeclExpr(LocalVariableDeclExpr *localVariableDeclExpr) {
    for (Expr*& argument : localVariableDeclExpr->initializerArgs) {
        processExpr(argument);
    }

    trackLocalVariable(localVariableDeclExpr);
}

void Lifetimes::processMemberAccessCallExpr(MemberAccessCallExpr* memberAccessCallExpr) {
    processExpr(memberAccessCallExpr->objectRef);
}

void Lifetimes::processParenExpr(ParenExpr *parenExpr) {
    processExpr(parenExpr->containedExpr);
}

void Lifetimes::processPostfixOperatorExpr(PostfixOperatorExpr *postfixOperatorExpr) {
    processExpr(postfixOperatorExpr->expr);
}

void Lifetimes::processPrefixOperatorExpr(PrefixOperatorExpr *prefixOperatorExpr) {
    processExpr(prefixOperatorExpr->expr);
}

void Lifetimes::processStringLiteralExpr(StringLiteralExpr *stringLiteralExpr) {

}

void Lifetimes::processTernaryExpr(TernaryExpr *ternaryExpr) {
    processExpr(ternaryExpr->condition);
    processExpr(ternaryExpr->trueExpr);
    processExpr(ternaryExpr->falseExpr);
}

DestructLocalVariableExpr *Lifetimes::destructLocalVariable(LocalVariableDeclExpr *localVariableDeclExpr) {
    if (!llvm::isa<ResolvedTypeRefExpr>(localVariableDeclExpr->type)) {
        // CodeVerifier will handle this
        return nullptr;
    }

    auto resolvedTypeRef = llvm::dyn_cast<ResolvedTypeRefExpr>(localVariableDeclExpr->type);

    Type* checkType = resolvedTypeRef->resolvedType;

    if (llvm::isa<ConstType>(checkType)) {
        checkType = llvm::dyn_cast<ConstType>(checkType)->pointToType;
    } else if (llvm::isa<MutType>(checkType)) {
        checkType = llvm::dyn_cast<MutType>(checkType)->pointToType;
    } else if (llvm::isa<ImmutType>(checkType)) {
        checkType = llvm::dyn_cast<ImmutType>(checkType)->pointToType;
    }

    if (llvm::isa<StructType>(checkType)) {
        auto structType = llvm::dyn_cast<StructType>(checkType);
        auto foundDestructor = structType->decl()->destructor;

        // TODO: In the future when we support creating default destructors we will need to just skip structs that
        //  don't have destructors (because not having a destructor will mean it didn't need a destructor)
        if (foundDestructor == nullptr) {
            printError("[INTERNAL] struct `" + structType->decl()->name() + "` is missing a destructor!",
                       localVariableDeclExpr->startPosition(), localVariableDeclExpr->endPosition());
        }

        RefLocalVariableExpr *refLocal = new RefLocalVariableExpr({}, {}, localVariableDeclExpr->name());
        refLocal->resultType = localVariableDeclExpr->resultType->deepCopy();
        // A local variable reference is an lvalue.
        refLocal->resultType->setIsLValue(true);
        // TODO: Do we need to dereference the local variable reference? I don't think we do...

        if (foundDestructor != nullptr) {
            currentFileAst->addImportExtern(foundDestructor);
        }

        return new DestructLocalVariableExpr({}, {}, refLocal, foundDestructor);
    } else {
        return nullptr;
    }
}

DestructParameterExpr *Lifetimes::destructParameter(int64_t paramIndex, ParameterDecl* parameter) {
    Type* checkType = parameter->type;

    if (llvm::isa<ConstType>(checkType)) {
        checkType = llvm::dyn_cast<ConstType>(checkType)->pointToType;
    } else if (llvm::isa<MutType>(checkType)) {
        checkType = llvm::dyn_cast<MutType>(checkType)->pointToType;
    } else if (llvm::isa<ImmutType>(checkType)) {
        checkType = llvm::dyn_cast<ImmutType>(checkType)->pointToType;
    }

    if (llvm::isa<StructType>(checkType)) {
        auto structType = llvm::dyn_cast<StructType>(checkType);
        auto foundDestructor = structType->decl()->destructor;

        // TODO: In the future when we support creating default destructors we will need to just skip structs that
        //  don't have destructors (because not having a destructor will mean it didn't need a destructor)
        if (foundDestructor == nullptr) {
            printError("[INTERNAL] struct `" + structType->decl()->name() + "` is missing a destructor!",
                       parameter->startPosition(), parameter->endPosition());
        }

        RefParameterExpr* refParameter = new RefParameterExpr({}, {}, paramIndex);
        refParameter->resultType = parameter->type->deepCopy();
        // A local variable reference is an lvalue.
        refParameter->resultType->setIsLValue(true);
        // TODO: Do we need to dereference the local variable reference? I don't think we do...

        if (foundDestructor != nullptr) {
            currentFileAst->addImportExtern(foundDestructor);
        }

        return new DestructParameterExpr({}, {}, refParameter, foundDestructor);
    } else {
        return nullptr;
    }
}

void Lifetimes::destructLocalVariablesDeclaredAfterLoop(Stmt *loop, std::vector<Expr*>& addToList) {
    unsigned int numLocalVariables = 0;

    if (llvm::isa<ForStmt>(loop)) {
        numLocalVariables = llvm::dyn_cast<ForStmt>(loop)->currentNumLocalVariables;
    } else if (llvm::isa<DoStmt>(loop)) {
        numLocalVariables = llvm::dyn_cast<ForStmt>(loop)->currentNumLocalVariables;
    } else if (llvm::isa<WhileStmt>(loop)) {
        numLocalVariables = llvm::dyn_cast<ForStmt>(loop)->currentNumLocalVariables;
    }

    // If we've created new local variables then we have to destruct them
    for (std::int64_t i = static_cast<int64_t>(currentLocalVariablesCount) - 1; i >= numLocalVariables; --i) {
        // Add the destructor call to the end of the compound statement...
        DestructLocalVariableExpr *destructLocalVariableExpr = destructLocalVariable(currentLocalVariables[i]);

        if (destructLocalVariableExpr != nullptr) {
            addToList.push_back(destructLocalVariableExpr);
        }
    }
}
