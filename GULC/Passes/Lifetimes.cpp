// Copyright (C) 2019 Michael Brandon Huddle
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as published
// by the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public License
// along with this program.  If not, see <https://www.gnu.org/licenses/>.

#include <AST/Exprs/BinaryOperatorExpr.hpp>
#include <AST/Decls/StructDecl.hpp>
#include <AST/Types/StructType.hpp>
#include <AST/Exprs/RefLocalVariableExpr.hpp>
#include <AST/Types/ReferenceType.hpp>
#include <ASTHelpers/VisibilityChecker.hpp>
#include <AST/Exprs/BaseDestructorCallExpr.hpp>
#include <AST/Exprs/ReconstructExpr.hpp>
#include <AST/Exprs/LValueToRValueExpr.hpp>
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

            if (llvm::isa<StructType>(checkType)) {
                auto structType = llvm::dyn_cast<StructType>(checkType);

                if (structType->decl()->destructor == nullptr) {
                    printError("[INTERNAL] struct `" + structType->decl()->name() + "` is missing a destructor!",
                               memberVariable->startPosition(), memberVariable->endPosition());
                }

                // TODO: Clean this up. A lot of this code already exists in `DeclResolver`...
                StructType *thisType = new StructType({}, {}, TypeQualifier::None, currentStruct->name(), currentStruct);

                RefParameterExpr *refThisParam = new RefParameterExpr({}, {}, 0);
                refThisParam->resultType = new ReferenceType({}, {}, TypeQualifier::None, thisType->deepCopy());
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

        // The last thing a destructor needs to do is call the base destructor if it exists
        if (currentStruct->destructor->baseDestructor != nullptr) {
            returnStmt->preReturnExprs.push_back(new BaseDestructorCallExpr({}, {},
                                                                            currentStruct->destructor->baseDestructor));
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

    processExpr(binaryOperatorExpr->leftValue);
    processExpr(binaryOperatorExpr->rightValue);

    if (binaryOperatorExpr->operatorName() == "=") {
        // If the binary operator is an assignment operator and the left value is a struct
        // then we have to call the destructor on the left value and perform either a copy
        // or a move on the right value
        if (llvm::isa<StructType>(binaryOperatorExpr->leftValue->resultType)) {
            auto structType = llvm::dyn_cast<StructType>(binaryOperatorExpr->leftValue->resultType);
            // TODO: Detect if we should move or copy
            auto useConstructor = structType->decl()->copyConstructor;

            if (useConstructor == nullptr) {
                // We CANNOT assign a struct if it doesn't have the required constructor
                printError("struct type `" + structType->decl()->name() + "` has deleted copy constructor!",
                           binaryOperatorExpr->leftValue->startPosition(),
                           binaryOperatorExpr->leftValue->endPosition());
            }

            if (llvm::isa<LocalVariableDeclExpr>(binaryOperatorExpr->leftValue)) {
                // If the left value is a local variable declaration then we can easily replace `expr` with the
                // local variable declaration that will have the right value be its initializer, setting the
                // found constructor to the copy/move constructor
                auto localVariableDecl = llvm::dyn_cast<LocalVariableDeclExpr>(binaryOperatorExpr->leftValue);

                if (localVariableDecl->hasInitializer()) {
                    // I believe we already check for this somewhere else but it doesn't hurt to verify here
                    // This prevents `StructType ex(2, 33) = otherStructVar;` which just doesn't look right.
                    printError("local variables cannot have both initializer arguments and an initial assignment!",
                               localVariableDecl->startPosition(), localVariableDecl->endPosition());
                }

                // TODO: Is the right value an rvalue? Is that okay? I believe it will always be an rvalue at this point
                localVariableDecl->initializerArgs = {
                        binaryOperatorExpr->rightValue
                };

                localVariableDecl->foundConstructor = useConstructor;

                // Safely delete `binaryOperatorExpr` and remove references to the left and right values since we steal
                // them.
                binaryOperatorExpr->leftValue = nullptr;
                binaryOperatorExpr->rightValue = nullptr;
                delete binaryOperatorExpr;

                // Replace the old binary operator with our local variable declaration that now has a copy or move
                expr = localVariableDecl;
            } else {
                // `rightValue` is most likely an `LValueToRValueExpr` at this point. If it is we remove that to
                // prevent issues as it is supposed to be an lvalue.
                if (llvm::isa<LValueToRValueExpr>(binaryOperatorExpr->rightValue)) {
                    LValueToRValueExpr* lvalueToRValue = llvm::dyn_cast<LValueToRValueExpr>(binaryOperatorExpr->rightValue);

                    // Make the right value a proper lvalue again
                    binaryOperatorExpr->rightValue = lvalueToRValue->lvalue;

                    // Delete the unused lvalue to rvalue conversion expression
                    lvalueToRValue->lvalue = nullptr;
                    delete lvalueToRValue;
                }

                // If the left value is NOT a local variable declaration then we will replace `expr` with a
                // constructor call with `this` being set to the left value and `other` being set to the right value
                std::vector<Expr*> arguments = {
                        binaryOperatorExpr->rightValue
                };

                // Create a reconstruction expression. This will call the constructor we provide and call the
                // constructor on the already constructed value `this` with the `other` argument. We do this instead
                // of having copy/move assignment operators
                auto reconstructExpr = new ReconstructExpr(binaryOperatorExpr->startPosition(),
                                                           binaryOperatorExpr->endPosition(),
                                                           useConstructor, binaryOperatorExpr->leftValue,
                                                           arguments, true);

                // Safely delete `binaryOperatorExpr` and remove references to the left and right values since we steal
                // them.
                binaryOperatorExpr->leftValue = nullptr;
                binaryOperatorExpr->rightValue = nullptr;
                delete binaryOperatorExpr;

                // Replace the old binary operator with our reconstruct expression
                expr = reconstructExpr;
            }
        }
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

    if (llvm::isa<StructType>(checkType)) {
        auto structType = llvm::dyn_cast<StructType>(checkType);
        auto foundDestructor = structType->decl()->destructor;

        if (foundDestructor == nullptr) {
            printError("[INTERNAL] struct `" + structType->decl()->name() + "` is missing a destructor!",
                       localVariableDeclExpr->startPosition(), localVariableDeclExpr->endPosition());
        }

        RefLocalVariableExpr *refLocal = new RefLocalVariableExpr({}, {}, localVariableDeclExpr->name());
        refLocal->resultType = localVariableDeclExpr->resultType->deepCopy();
        // A local variable reference is an lvalue.
        refLocal->resultType->setIsLValue(true);

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

    if (llvm::isa<StructType>(checkType)) {
        auto structType = llvm::dyn_cast<StructType>(checkType);
        auto foundDestructor = structType->decl()->destructor;

        if (foundDestructor == nullptr) {
            printError("[INTERNAL] struct `" + structType->decl()->name() + "` is missing a destructor!",
                       parameter->startPosition(), parameter->endPosition());
        }

        RefParameterExpr* refParameter = new RefParameterExpr({}, {}, paramIndex);
        refParameter->resultType = parameter->type->deepCopy();
        // A local variable reference is an lvalue.
        refParameter->resultType->setIsLValue(true);

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
