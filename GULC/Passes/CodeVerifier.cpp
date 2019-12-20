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
#include <AST/Types/PointerType.hpp>
#include <AST/Types/BuiltInType.hpp>
#include <AST/Types/ReferenceType.hpp>
#include <ASTHelpers/TypeComparer.hpp>
#include <AST/Types/UnresolvedType.hpp>
#include <AST/Exprs/UnresolvedTypeRefExpr.hpp>
#include <AST/Types/StructType.hpp>
#include <ASTHelpers/VisibilityChecker.hpp>
#include <ASTHelpers/FunctionComparer.hpp>
#include "CodeVerifier.hpp"
#include "DeclResolver.hpp"

using namespace gulc;

void CodeVerifier::verifyFile(FileAST* fileAst) {
    currentFileAst = fileAst;

    for (Decl* decl : fileAst->topLevelDecls()) {
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
            if (!isExplicit) {
                // Implicitly casting from pointer to built in is NOT supported
                return false;
            }

            // Cannot cast from pointer to floating or to signed (this is to prevent potential issues with how signed numbers are treated)
            // Though, nothing is stopping someone from casting like: `(double)(ulong)&ptr`
            return !(toBuiltIn->isFloating() || toBuiltIn->isSigned());
        }
    } else if (llvm::isa<ReferenceType>(to)) {
        if (llvm::isa<ReferenceType>(from)) {
            auto toRef = llvm::dyn_cast<ReferenceType>(to);
            auto fromRef = llvm::dyn_cast<ReferenceType>(from);

            return canCastType(toRef->referenceToType, fromRef->referenceToType, isExplicit);
        } else {
            return false;
        }
    }

    return false;
}

bool CodeVerifier::typeIsAssignable(Type* checkType) {
    // TODO: We should make something like `isLValue` or `isRValue` a member of `Type` so we can detect assignability with that as well...
    return checkType->qualifier() != TypeQualifier::Const;
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
        case Decl::Kind::CallOperator:
            verifyCallOperatorDecl(llvm::dyn_cast<CallOperatorDecl>(decl));
            break;
        case Decl::Kind::CastOperator:
            verifyCastOperatorDecl(llvm::dyn_cast<CastOperatorDecl>(decl));
            break;
        case Decl::Kind::Enum:
            // I don't think we currently have anything to verify here
            break;
        case Decl::Kind::Function:
            verifyFunctionDecl(llvm::dyn_cast<FunctionDecl>(decl));
            break;
        case Decl::Kind::GlobalVariable:
            verifyGlobalVariableDecl(llvm::dyn_cast<GlobalVariableDecl>(decl));
            break;
        case Decl::Kind::IndexOperator:
            verifyIndexOperatorDecl(llvm::dyn_cast<IndexOperatorDecl>(decl));
            break;
        case Decl::Kind::Namespace:
            verifyNamespaceDecl(llvm::dyn_cast<NamespaceDecl>(decl));
            break;
        case Decl::Kind::Operator:
            verifyOperatorDecl(llvm::dyn_cast<OperatorDecl>(decl));
            break;
        case Decl::Kind::Struct:
            verifyStructDecl(llvm::dyn_cast<StructDecl>(decl));
            break;
        case Decl::Kind::TemplateFunction:
            verifyTemplateFunctionDecl(llvm::dyn_cast<TemplateFunctionDecl>(decl));
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
        case Expr::Kind::AssignmentBinaryOperator:
            verifyAssignmentBinaryOperatorExpr(llvm::dyn_cast<AssignmentBinaryOperatorExpr>(expr));
            break;
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
        case Expr::Kind::PotentialLocalVariableDecl:
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
void CodeVerifier::verifyCallOperatorDecl(CallOperatorDecl *callOperatorDecl) {
    for (Decl* checkDecl : callOperatorDecl->parentStruct->members) {
        if (checkDecl == callOperatorDecl) {
            continue;
        }

        if (llvm::isa<CallOperatorDecl>(checkDecl)) {
            auto checkCallOperatorDecl = llvm::dyn_cast<CallOperatorDecl>(checkDecl);

            if (FunctionComparer::compareParams(callOperatorDecl->parameters, checkCallOperatorDecl->parameters) ==
                FunctionComparer::CompareResult::Identical) {
                printError("`operator ()` with the provided arguments already exists!",
                           callOperatorDecl->startPosition(), callOperatorDecl->endPosition());
            }
        }
    }

    currentFunctionReturnType = callOperatorDecl->resultType;
    currentFunctionParameters = &callOperatorDecl->parameters;

    currentFunctionLocalVariablesCount = 0;

    validateGotoVariables.clear();

    verifyCompoundStmt(callOperatorDecl->body());

    currentFunctionLocalVariablesCount = 0;

    currentFunctionReturnType = nullptr;
    currentFunctionParameters = nullptr;
}

void CodeVerifier::verifyCastOperatorDecl(CastOperatorDecl *castOperatorDecl) {
    for (Decl* checkDecl : castOperatorDecl->parentStruct->members) {
        if (checkDecl == castOperatorDecl) {
            continue;
        }

        if (llvm::isa<CastOperatorDecl>(checkDecl)) {
            auto checkCastOperatorDecl = llvm::dyn_cast<CastOperatorDecl>(checkDecl);

            auto thisType = castOperatorDecl->resultType;
            auto checkType = checkCastOperatorDecl->resultType;

            // We ignore if it is a reference or not
            if (llvm::isa<ReferenceType>(thisType)) {
                thisType = llvm::dyn_cast<ReferenceType>(thisType)->referenceToType;
            }

            if (llvm::isa<ReferenceType>(checkType)) {
                checkType = llvm::dyn_cast<ReferenceType>(checkType)->referenceToType;
            }

            // If the return types aren't the same then we continue looking, this is different than every other function
            if (!TypeComparer::getTypesAreSame(thisType, checkType, false)) {
                continue;
            }

            if (castOperatorDecl->castOperatorType() != checkCastOperatorDecl->castOperatorType()) {
                printError("defining both `explicit` and `implicit` cast operators for the same type is not allowed!",
                           castOperatorDecl->startPosition(), castOperatorDecl->endPosition());
            } else {
                printError("cast operator for the type `" + castOperatorDecl->resultType->getString() +
                           "` already exists!",
                           castOperatorDecl->startPosition(), castOperatorDecl->endPosition());
            }
        }
    }

    currentFunctionReturnType = castOperatorDecl->resultType;
    currentFunctionParameters = &castOperatorDecl->parameters;

    currentFunctionLocalVariablesCount = 0;

    validateGotoVariables.clear();

    verifyCompoundStmt(castOperatorDecl->body());

    currentFunctionLocalVariablesCount = 0;

    currentFunctionReturnType = nullptr;
    currentFunctionParameters = nullptr;
}

void CodeVerifier::verifyConstructorDecl(ConstructorDecl *constructorDecl) {
    // Verify a constructor with the same signature doesn't already exist...
    for (ConstructorDecl* checkConstructor : currentStruct->constructors) {
        if (checkConstructor != constructorDecl) {
            if (FunctionComparer::compareParams(constructorDecl->parameters, checkConstructor->parameters) ==
                    FunctionComparer::CompareResult::Identical) {
                printError("a constructor with the same signature already exists!",
                           constructorDecl->startPosition(), constructorDecl->endPosition());
            }
        }
    }

    currentFunctionReturnType = nullptr;
    currentFunctionParameters = &constructorDecl->parameters;

    currentFunctionLocalVariablesCount = 0;

    verifyCompoundStmt(constructorDecl->body());

    currentFunctionLocalVariablesCount = 0;

    currentFunctionParameters = nullptr;
}

void CodeVerifier::verifyFunctionDecl(FunctionDecl *functionDecl) {
    if (checkFunctionExists(functionDecl)) {
        printError("function name '" + functionDecl->name() + "' is ambiguous with the current parameter types!",
                   functionDecl->startPosition(), functionDecl->endPosition());
        return;
    } else if (checkDeclNameInUse(functionDecl->name(), functionDecl, true)) {
        printError("redefinition for `" + functionDecl->name() + "` as a different declaration is not supported!",
                   functionDecl->startPosition(), functionDecl->endPosition());
        return;
    }

    currentFunctionReturnType = functionDecl->resultType;
    currentFunctionParameters = &functionDecl->parameters;

    currentFunctionLocalVariablesCount = 0;

    validateGotoVariables.clear();

    verifyCompoundStmt(functionDecl->body());

    currentFunctionLocalVariablesCount = 0;

    currentFunctionReturnType = nullptr;
    currentFunctionParameters = nullptr;
}

void CodeVerifier::verifyGlobalVariableDecl(GlobalVariableDecl *globalVariableDecl) {
    if (checkDeclNameInUse(globalVariableDecl->name(), globalVariableDecl)) {
        printError("redefinition of name `" + globalVariableDecl->name() + "` is not allowed!",
                   globalVariableDecl->startPosition(), globalVariableDecl->endPosition());
    }

    // TODO: We also need to verify the initial value is a constant/literal value...
    if (globalVariableDecl->hasInitialValue()) {
        verifyExpr(globalVariableDecl->initialValue);
    }
}

void CodeVerifier::verifyIndexOperatorDecl(IndexOperatorDecl *indexOperatorDecl) {
    for (Decl* checkDecl : indexOperatorDecl->parentStruct->members) {
        if (checkDecl == indexOperatorDecl) {
            continue;
        }

        if (llvm::isa<IndexOperatorDecl>(checkDecl)) {
            auto checkIndexOperatorDecl = llvm::dyn_cast<IndexOperatorDecl>(checkDecl);

            if (FunctionComparer::compareParams(indexOperatorDecl->parameters, checkIndexOperatorDecl->parameters) ==
                    FunctionComparer::CompareResult::Identical) {
                printError("indexer with the provided arguments already exists!",
                           indexOperatorDecl->startPosition(), indexOperatorDecl->endPosition());
            }
        }
    }

    currentFunctionReturnType = indexOperatorDecl->resultType;
    currentFunctionParameters = &indexOperatorDecl->parameters;

    currentFunctionLocalVariablesCount = 0;

    validateGotoVariables.clear();

    verifyCompoundStmt(indexOperatorDecl->body());

    currentFunctionLocalVariablesCount = 0;

    currentFunctionReturnType = nullptr;
    currentFunctionParameters = nullptr;
}

void CodeVerifier::verifyNamespaceDecl(NamespaceDecl *namespaceDecl) {
    NamespaceDecl* oldNamespace = currentNamespace;
    currentNamespace = namespaceDecl;

    for (Decl* decl : namespaceDecl->nestedDecls()) {
        verifyDecl(decl);
    }

    currentNamespace = oldNamespace;
}

void CodeVerifier::verifyOperatorDecl(OperatorDecl *operatorDecl) {
    if (checkOperatorExists(operatorDecl)) {
        printError("operator '" + operatorDecl->operatorName() + "' is ambiguous for type `" +
                   operatorDecl->parentStruct->name() + "` with the current parameter types!",
                   operatorDecl->startPosition(), operatorDecl->endPosition());
        return;
    }

    std::string opName = operatorDecl->operatorName();

    switch (operatorDecl->operatorType()) {
        case OperatorType::Prefix:
            if (!operatorDecl->parameters.empty()) {
                printError("prefix operator '" + operatorDecl->operatorName() + "' cannot have parameters!",
                           operatorDecl->startPosition(), operatorDecl->endPosition());
            }
            break;
        case OperatorType::Infix:
            if (operatorDecl->parameters.size() > 1) {
                printError("infix operator '" + operatorDecl->operatorName() + "' cannot have more than 1 parameter!",
                           operatorDecl->startPosition(), operatorDecl->endPosition());
            }
            break;
        case OperatorType::Postfix:
            if (!operatorDecl->parameters.empty()) {
                printError("postfix operator '" + operatorDecl->operatorName() + "' cannot have parameters!",
                           operatorDecl->startPosition(), operatorDecl->endPosition());
            }

            if (operatorDecl->operatorName() != "++" && operatorDecl->operatorName() != "--") {
                printError("only postfix operators `++` and `--` are allowed, custom postfix operators are not supported!",
                           operatorDecl->startPosition(), operatorDecl->endPosition());
            }
            break;
        default:
            printError("unknown operator type!", operatorDecl->startPosition(), operatorDecl->endPosition());
            break;
    }

    currentFunctionReturnType = operatorDecl->resultType;
    currentFunctionParameters = &operatorDecl->parameters;

    currentFunctionLocalVariablesCount = 0;

    validateGotoVariables.clear();

    verifyCompoundStmt(operatorDecl->body());

    currentFunctionLocalVariablesCount = 0;

    currentFunctionReturnType = nullptr;
    currentFunctionParameters = nullptr;
}

void CodeVerifier::verifyStructDecl(StructDecl *structDecl) {
    StructDecl* oldStruct = currentStruct;
    currentStruct = structDecl;

    for (ConstructorDecl* constructor : structDecl->constructors) {
        verifyConstructorDecl(constructor);
    }

    // TODO: We should make sure any members of the struct can actually be members of the struct...
    // TODO: We will have to verify any of our member value-types do NOT reference our type. This is an expensive operation that will have to deep search the types
    //  we should be able to speed this up in most scenarios by not requiring it for external dependencies (expect templated types, they can still contain references to our type)
    for (Decl* decl : structDecl->members) {
        verifyDecl(decl);
    }

    currentStruct = oldStruct;
}

void CodeVerifier::verifyTemplateFunctionDecl(TemplateFunctionDecl *templateFunctionDecl) {
    currentFunctionTemplateParameters = &templateFunctionDecl->templateParameters;

    // NOTE: I don't think this is needed since we check if a name is taken in the `DeclResolver`
//    if (templateFunctionDecl->hasTemplateParameters() && !templateFunctionDecl->parameters.empty()) {
//        for (TemplateParameterDecl* templateParameter : templateFunctionDecl->templateParameters) {
//            for (ParameterDecl* parameter : templateFunctionDecl->parameters) {
//                if (templateParameter->name() == parameter->name()) {
//                    printError("parameter `" + parameter->name() + "` shadows template parameter of the same name!",
//                               parameter->startPosition(), parameter->endPosition());
//                    return;
//                }
//            }
//        }
//    }

    for (FunctionDecl* implementedFunction : templateFunctionDecl->implementedFunctions()) {
        verifyFunctionDecl(implementedFunction);
    }

    currentFunctionTemplateParameters = nullptr;
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
    unsigned int oldLocalVariableCount = currentFunctionLocalVariablesCount;

    for (Stmt* stmt : compoundStmt->statements()) {
        verifyStmt(stmt);
    }

    currentFunctionLocalVariablesCount = oldLocalVariableCount;

    for (GotoStmt* gotoStmt : validateGotoVariables) {
        // Only lower the number of local variables, never raise it
        // This is to make it so we find the common parent between the `goto` and the labeled statement
        if (currentFunctionLocalVariablesCount < gotoStmt->currentNumLocalVariables) {
            gotoStmt->currentNumLocalVariables = currentFunctionLocalVariablesCount;
        }
    }
}

void CodeVerifier::verifyContinueStmt(ContinueStmt *continueStmt) {
    // TODO: I don't think we need to do anything here. The labels have already been verified in `DeclResolver`?
}

void CodeVerifier::verifyDoStmt(DoStmt *doStmt) {
    if (doStmt->loopStmt != nullptr) verifyStmt(doStmt->loopStmt);
    // TODO: Should we verify the condition result is a boolean?
    verifyExpr(doStmt->condition);
}

void CodeVerifier::verifyForStmt(ForStmt *forStmt) {
    unsigned int oldLocalVariableCount = currentFunctionLocalVariablesCount;

    if (forStmt->preLoop != nullptr) verifyExpr(forStmt->preLoop);
    if (forStmt->condition != nullptr) verifyExpr(forStmt->condition);
    if (forStmt->iterationExpr != nullptr) verifyExpr(forStmt->iterationExpr);

    if (forStmt->loopStmt != nullptr) verifyStmt(forStmt->loopStmt);

    currentFunctionLocalVariablesCount = oldLocalVariableCount;

    // We also perform this operation after `for` loops to remove the number of variables declared in the preloop
    for (GotoStmt* gotoStmt : validateGotoVariables) {
        // Only lower the number of local variables, never raise it
        // This is to make it so we find the common parent between the `goto` and the labeled statement
        if (currentFunctionLocalVariablesCount < gotoStmt->currentNumLocalVariables) {
            gotoStmt->currentNumLocalVariables = currentFunctionLocalVariablesCount;
        }
    }
}

void CodeVerifier::verifyGotoStmt(GotoStmt *gotoStmt) {
    gotoStmt->currentNumLocalVariables = currentFunctionLocalVariablesCount;
    validateGotoVariables.push_back(gotoStmt);
}

void CodeVerifier::verifyIfStmt(IfStmt *ifStmt) {
    // TODO: We should support stuff like `if (Widget^ w = new Button("Test")) {}`
    if (ifStmt->condition != nullptr) verifyExpr(ifStmt->condition);

    if (ifStmt->trueStmt != nullptr) verifyStmt(ifStmt->trueStmt);

    if (ifStmt->hasFalseStmt()) {
        verifyStmt(ifStmt->falseStmt);
    }
}

void CodeVerifier::verifyLabeledStmt(LabeledStmt *labeledStmt) {
    // Validate any `goto` statements that reference this labeled statement.
    for (GotoStmt* gotoStmt : validateGotoVariables) {
        if (gotoStmt->label == labeledStmt->label()) {
            if (currentFunctionLocalVariablesCount > gotoStmt->currentNumLocalVariables) {
                printError("cannot jump from this goto to the referenced label, jump skips variable declarations!",
                           gotoStmt->startPosition(), gotoStmt->endPosition());
                return;
            }
        }
    }

    // The label has already been checked for ambiguity in `DeclResolver`
    verifyStmt(labeledStmt->labeledStmt);
}

void CodeVerifier::verifyReturnStmt(ReturnStmt *returnStmt) {
    // TODO: Should we verify the return value result type is the same as the result type of the current function?
    if (returnStmt->hasReturnValue()) {
        verifyExpr(returnStmt->returnValue);
    }
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
    if (whileStmt->loopStmt != nullptr) verifyStmt(whileStmt->loopStmt);
}

// Exprs
void CodeVerifier::verifyAssignmentBinaryOperatorExpr(AssignmentBinaryOperatorExpr *assignmentBinaryOperatorExpr) {
    verifyExpr(assignmentBinaryOperatorExpr->leftValue);
    verifyExpr(assignmentBinaryOperatorExpr->rightValue);

    // If the left value of the assignment is a local variable we check that the local variable declaration doesn't
    // have an initializer (this is to prevent doing `Example i(12) = 33;` which can look weird to new developers)
    if (llvm::isa<LocalVariableDeclExpr>(assignmentBinaryOperatorExpr->leftValue)) {
        auto localVariableDecl = llvm::dyn_cast<LocalVariableDeclExpr>(assignmentBinaryOperatorExpr->leftValue);

        if (assignmentBinaryOperatorExpr->hasNestedOperator()) {
            printError("local variable declarations cannot be used in any other binary operator expressions besides '='!",
                       assignmentBinaryOperatorExpr->startPosition(), assignmentBinaryOperatorExpr->endPosition());
        }

        if (localVariableDecl->hasInitializer()) {
            printError("having both an intializer and an initial assignment with `=` is not allowed!",
                       assignmentBinaryOperatorExpr->startPosition(), assignmentBinaryOperatorExpr->endPosition());
        }
    // If the left value of the assignment isn't a local variable declaration then we check for assignability...
    // NOTE: The assignment operator is NEVER overloadable so we don't have to worry about someone making an overloaded
    //       const-qualified operator.
    } else if (!typeIsAssignable(assignmentBinaryOperatorExpr->leftValue->resultType)) {
        if (assignmentBinaryOperatorExpr->leftValue->resultType->qualifier() == TypeQualifier::Const) {
            printError("cannot assign to const-qualified left value!",
                       assignmentBinaryOperatorExpr->leftValue->startPosition(),
                       assignmentBinaryOperatorExpr->leftValue->endPosition());
            return;
        }

        printError("cannot assign to an rvalue!",
                   assignmentBinaryOperatorExpr->leftValue->startPosition(),
                   assignmentBinaryOperatorExpr->leftValue->endPosition());
        return;
    }
}

void CodeVerifier::verifyBinaryOperatorExpr(Expr *&expr) {
    auto binaryOperatorExpr = llvm::dyn_cast<BinaryOperatorExpr>(expr);

    verifyExpr(binaryOperatorExpr->leftValue);
    verifyExpr(binaryOperatorExpr->rightValue);
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
    printError("[INTERNAL] identifier found in code verifier!",
               identifierExpr->startPosition(), identifierExpr->endPosition());
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
    if (localVariableNameTaken(localVariableDeclExpr->name())) {
        printError("local variable '" + localVariableDeclExpr->name() + "' redeclared as different type!",
                   localVariableDeclExpr->startPosition(), localVariableDeclExpr->endPosition());
    }

    addLocalVariable(localVariableDeclExpr);

    if (llvm::isa<UnresolvedTypeRefExpr>(localVariableDeclExpr->type)) {
        auto unresolvedTypeRef = llvm::dyn_cast<UnresolvedTypeRefExpr>(localVariableDeclExpr->type);

        printError("variable type '" + unresolvedTypeRef->unresolvedType->getString() + "' was not found!",
                   localVariableDeclExpr->startPosition(), localVariableDeclExpr->endPosition());
    }

    if (llvm::isa<ResolvedTypeRefExpr>(localVariableDeclExpr->type)) {
        auto resolvedTypeRef = llvm::dyn_cast<ResolvedTypeRefExpr>(localVariableDeclExpr->type);

        if (llvm::isa<StructType>(resolvedTypeRef->resolvedType)) {
            auto structType = llvm::dyn_cast<StructType>(resolvedTypeRef->resolvedType);

            // We do allow struct variables to be declared without a constructor call
            if (localVariableDeclExpr->foundConstructor != nullptr) {
                if (!VisibilityChecker::canAccessStructMember(structType, currentStruct,
                                                              localVariableDeclExpr->foundConstructor)) {
                    printError("struct '" + structType->getString() +
                               "' does not have a publicly visible constructor with the specified arguments!",
                               localVariableDeclExpr->startPosition(), localVariableDeclExpr->endPosition());
                }
            } else if (localVariableDeclExpr->hasInitializer()) {
                // We should never make it to this point since `DeclResolver` should handle this but just in case
                // we print an error here if the local variable decl has an initializer but there wasn't a resolved
                // constructor for it
                printError("no valid public constructor found for the provided initializer arguments!",
                           localVariableDeclExpr->startPosition(), localVariableDeclExpr->endPosition());
            }
        }
    }
}

void CodeVerifier::verifyLocalVariableDeclOrPrefixOperatorCallExpr(Expr *&expr) {
    printError("[INTERNAL] found `PotentialLocalVariableDecl` in verifier, this is not supported!",
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

bool CodeVerifier::checkDeclNameInUse(const std::string &name, Decl* ignoreDecl, bool ignoreFunctions) {
    // Check current struct/class
    if (currentStruct != nullptr) {
        for (Decl* checkDecl : currentStruct->members) {
            if (ignoreDecl == checkDecl) continue;

            if (checkDecl->name() == name) {
                // If we're supposed to ignore functions and the `checkDecl` is a function or template function then we go to the next top level decl...
                if (ignoreFunctions &&
                    (llvm::isa<FunctionDecl>(checkDecl) || llvm::isa<TemplateFunctionDecl>(checkDecl))) {
                    continue;
                }

                return true;
            }
        }

        // If the current struct isn't null then we don't check the current namespace or current file file, we can differentiate between file and namespace declarations...
        return false;
    }

    // Check current namespace
    if (currentNamespace != nullptr) {
        for (Decl* checkDecl : currentNamespace->nestedDecls()) {
            if (ignoreDecl == checkDecl) continue;

            if (checkDecl->name() == name) {
                // If we're supposed to ignore functions and the `checkDecl` is a function or template function then we go to the next top level decl...
                if (ignoreFunctions &&
                    (llvm::isa<FunctionDecl>(checkDecl) || llvm::isa<TemplateFunctionDecl>(checkDecl))) {
                    continue;
                }

                return true;
            }
        }

        // If the current namespace isn't null then we don't check the current file, we can differentiate between file and namespace declarations...
        return false;
    }

    // Check current file
    for (Decl* checkDecl : currentFileAst->topLevelDecls()) {
        if (ignoreDecl == checkDecl) continue;

        if (checkDecl->name() == name) {
            // If we're supposed to ignore functions and the `checkDecl` is a function or template function then we go to the next top level decl...
            if (ignoreFunctions &&
                    (llvm::isa<FunctionDecl>(checkDecl) || llvm::isa<TemplateFunctionDecl>(checkDecl))) {
                continue;
            }

            return true;
        }
    }

    return false;
}

bool CodeVerifier::checkFunctionExists(FunctionDecl *function) {
    // Check current struct/class
    if (currentStruct != nullptr) {
        for (Decl* checkDecl : currentStruct->members) {
            if (function == checkDecl) continue;

            if (llvm::isa<FunctionDecl>(checkDecl)) {
                auto functionDecl = llvm::dyn_cast<FunctionDecl>(checkDecl);

                if (checkDecl->name() == function->name()) {
                    // If the parameters are the same then we return saying the function exists...
                    if (checkParamsAreSame(functionDecl->parameters, function->parameters)) {
                        return true;
                    }
                    // Else we keep searching...
                }
            }
        }

        // If the current struct isn't null then we don't check the current namespace or current file, we can differentiate between file and namespace functions when calling...
        return false;
    }

    // Check current namespace
    if (currentNamespace != nullptr) {
        for (Decl* checkDecl : currentNamespace->nestedDecls()) {
            if (function == checkDecl) continue;

            if (llvm::isa<FunctionDecl>(checkDecl)) {
                auto functionDecl = llvm::dyn_cast<FunctionDecl>(checkDecl);

                if (checkDecl->name() == function->name()) {
                    // If the parameters are the same then we return saying the function exists...
                    if (checkParamsAreSame(functionDecl->parameters, function->parameters)) {
                        return true;
                    }
                    // Else we keep searching...
                }
            }
        }

        // If the current namespace isn't null then we don't check the current file, we can differentiate between file and namespace functions when calling...
        return false;
    }

    // Check current file
    for (Decl* checkDecl : currentFileAst->topLevelDecls()) {
        // Skip if the `checkDecl` IS the `function` we're supposed to be checking...
        if (function == checkDecl) continue;

        if (llvm::isa<FunctionDecl>(checkDecl)) {
            auto functionDecl = llvm::dyn_cast<FunctionDecl>(checkDecl);

            if (checkDecl->name() == function->name()) {
                // If the parameters are the same then we return saying the function exists...
                if (checkParamsAreSame(functionDecl->parameters, function->parameters)) {
                    return true;
                }
                // Else we keep searching...
            }
        }
    }

    return false;
}

bool CodeVerifier::checkParamsAreSame(std::vector<ParameterDecl *> &params1, std::vector<ParameterDecl *> &params2) {
    // NOTE: We do NOT allow overloaded functions with optional values to conflict with functions that do not have optionals
    //  Ex: `test(int i)` and `test(int i, int j = 0)` CANNOT appear in the same context.
    //  This should NOT be an ambiguity error when you call `test`, it should be an error when generating one of the functions (depends on the order the functions compile in...)
    std::size_t checkLength = params1.size();

    if (params2.size() > checkLength) {
        checkLength = params2.size();
    }

    for (std::size_t i = 0; i < checkLength; ++i) {
        // If `i` is greater than or equal to the size of `param1` or `param2` then we check the opposite list for an optional value
        //  if there is an optional value at the index then we say the types are the same.
        if (i >= params1.size()) {
            return params2[i]->hasDefaultArgument();
        } else if (i >= params2.size()) {
            return params1[i]->hasDefaultArgument();
        } else {
            // If `i` is still within the range for both `param1` and `param2` then we check the equality of each param...
            Type* paramType1 = params1[i]->type;
            Type* paramType2 = params2[i]->type;

            // A function that has a reference type is called exactly the same way as a function without a reference type
            //  because of this we treat reference parameters exactly the same as non-reference parameters
            if (llvm::isa<ReferenceType>(paramType1)) paramType1 = llvm::dyn_cast<ReferenceType>(paramType1)->referenceToType;
            if (llvm::isa<ReferenceType>(paramType2)) paramType2 = llvm::dyn_cast<ReferenceType>(paramType2)->referenceToType;

            if (!TypeComparer::getTypesAreSame(paramType1, paramType2)) {
                return false;
            }
        }
    }

    // If we reach this point then the parameter lists are the same in terms of callability...
    return true;
}

bool CodeVerifier::checkOperatorExists(OperatorDecl *operatorDecl) {
    // Operators can only exist in `StructDecl` and `TraitDecl`
    for (Decl* checkDecl : currentStruct->members) {
        if (operatorDecl == checkDecl) continue;

        if (llvm::isa<OperatorDecl>(checkDecl)) {
            auto checkOperatorDecl = llvm::dyn_cast<OperatorDecl>(checkDecl);

            if (checkOperatorDecl->operatorName() == operatorDecl->operatorName()) {
                // If the parameters are the same then we return saying the operator exists...
                if (FunctionComparer::compareParams(checkOperatorDecl->parameters, operatorDecl->parameters) ==
                        FunctionComparer::CompareResult::Identical) {
                    return true;
                }
                // Else we keep searching...
            }
        }
    }

    return false;
}
