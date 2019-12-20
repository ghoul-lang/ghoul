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

#include <AST/Types/BuiltInType.hpp>
#include <AST/Types/PointerType.hpp>
#include <AST/Types/FunctionPointerType.hpp>
#include <AST/Exprs/LValueToRValueExpr.hpp>
#include <AST/Types/ReferenceType.hpp>
#include <AST/Exprs/RefLocalVariableExpr.hpp>
#include <AST/Exprs/RefParameterExpr.hpp>
#include <AST/Exprs/RefFunctionExpr.hpp>
#include <AST/Types/EnumType.hpp>
#include <AST/Exprs/TempNamespaceRefExpr.hpp>
#include <ASTHelpers/TypeComparer.hpp>
#include <AST/Exprs/UnresolvedTypeRefExpr.hpp>
#include <AST/Types/FunctionTemplateTypenameRefType.hpp>
#include <AST/Types/StructType.hpp>
#include <AST/Exprs/RefStructMemberVariableExpr.hpp>
#include <AST/Exprs/RefStructMemberFunctionExpr.hpp>
#include <ASTHelpers/VisibilityChecker.hpp>
#include <ASTHelpers/FunctionComparer.hpp>
#include <AST/Exprs/RefBaseExpr.hpp>
#include <AST/Stmts/ConstructStructMemberVariableStmt.hpp>
#include <make_reverse_iterator.hpp>
#include <AST/Decls/OperatorDecl.hpp>
#include <AST/Exprs/CustomInfixOperatorCallExpr.hpp>
#include <ASTHelpers/CastHelper.hpp>
#include <AST/Exprs/ConstructTemporaryValueExpr.hpp>
#include <AST/Exprs/InfixMacroCallExpr.hpp>
#include <AST/Exprs/CustomPrefixOperatorCallExpr.hpp>
#include <AST/Decls/CastOperatorDecl.hpp>
#include <AST/Decls/IndexOperatorDecl.hpp>
#include <AST/Exprs/CustomIndexOperatorCallExpr.hpp>
#include <AST/Decls/CallOperatorDecl.hpp>
#include <AST/Exprs/CustomCallOperatorCallExpr.hpp>
#include "DeclResolver.hpp"
#include "TypeResolver.hpp"

using namespace gulc;

void DeclResolver::processFile(std::vector<FileAST*>& files) {
    for (FileAST* fileAst : files) {
        currentFileAst = fileAst;
        currentImports = &fileAst->imports();

        for (Decl *decl : fileAst->topLevelDecls()) {
            processDecl(decl);
        }
    }
}

bool DeclResolver::getTypeIsReference(const Type* check) {
    return llvm::isa<ReferenceType>(check);
}

void DeclResolver::printError(const std::string &message, TextPosition startPosition, TextPosition endPosition) {
    std::cout << "gulc resolver error[" << currentFileAst->filePath() << ", "
                                     "{" << startPosition.line << ", " << startPosition.column << "} "
                                     "to {" << endPosition.line << ", " << endPosition.column << "}]: "
              << message
              << std::endl;
    std::exit(1);
}

void DeclResolver::printWarning(const std::string &message, TextPosition startPosition, TextPosition endPosition) {
    std::cout << "gulc warning[" << currentFileAst->filePath() << ", "
                              "{" << startPosition.line << ", " << startPosition.column << "} "
                              "to {" << endPosition.line << ", " << endPosition.column << "}]: "
              << message
              << std::endl;
}

void DeclResolver::printDebugWarning(const std::string &message) {
#ifndef NDEBUG
    std::cout << "gulc resolver [DEBUG WARNING](" << currentFileAst->filePath() << "): " << message << std::endl;
#endif
}

void DeclResolver::processDecl(Decl *decl) {
    switch (decl->getDeclKind()) {
        case Decl::Kind::Enum:
            processEnumDecl(llvm::dyn_cast<EnumDecl>(decl));
            break;
        // There isn't anything special about operator vs function in this pass so we just treat it like a function
        case Decl::Kind::Operator:
        case Decl::Kind::CastOperator:
        case Decl::Kind::CallOperator:
        case Decl::Kind::IndexOperator:
        case Decl::Kind::Function:
            processFunctionDecl(llvm::dyn_cast<FunctionDecl>(decl));
            break;
        case Decl::Kind::GlobalVariable:
            processGlobalVariableDecl(llvm::dyn_cast<GlobalVariableDecl>(decl));
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
        case Decl::Kind::Parameter:
        case Decl::Kind::TemplateParameter:
        default:
            printDebugWarning("unhandled Decl in 'processDecl'!");
            break;
    }
}

bool DeclResolver::processStmt(Stmt *&stmt) {
    switch (stmt->getStmtKind()) {
        case Stmt::Kind::Break:
            return processBreakStmt(llvm::dyn_cast<BreakStmt>(stmt));
        case Stmt::Kind::Case:
            return processCaseStmt(llvm::dyn_cast<CaseStmt>(stmt));
        case Stmt::Kind::Compound:
            return processCompoundStmt(llvm::dyn_cast<CompoundStmt>(stmt), false);
        case Stmt::Kind::Continue:
            return processContinueStmt(llvm::dyn_cast<ContinueStmt>(stmt));
        case Stmt::Kind::Do:
            return processDoStmt(llvm::dyn_cast<DoStmt>(stmt));
        case Stmt::Kind::For:
            return processForStmt(llvm::dyn_cast<ForStmt>(stmt));
        case Stmt::Kind::Goto:
            return processGotoStmt(llvm::dyn_cast<GotoStmt>(stmt));
        case Stmt::Kind::If:
            return processIfStmt(llvm::dyn_cast<IfStmt>(stmt));
        case Stmt::Kind::Labeled:
            return processLabeledStmt(llvm::dyn_cast<LabeledStmt>(stmt));
        case Stmt::Kind::Return:
            return processReturnStmt(llvm::dyn_cast<ReturnStmt>(stmt));
        case Stmt::Kind::Switch:
            return processSwitchStmt(llvm::dyn_cast<SwitchStmt>(stmt));
        case Stmt::Kind::Try:
            return processTryStmt(llvm::dyn_cast<TryStmt>(stmt));
        case Stmt::Kind::TryCatch:
            return processTryCatchStmt(llvm::dyn_cast<TryCatchStmt>(stmt));
        case Stmt::Kind::TryFinally:
            return processTryFinallyStmt(llvm::dyn_cast<TryFinallyStmt>(stmt));
        case Stmt::Kind::While:
            return processWhileStmt(llvm::dyn_cast<WhileStmt>(stmt));
        case Stmt::Kind::Expr: {
            auto expr = llvm::dyn_cast<Expr>(stmt);
            processExpr(expr);
            stmt = expr;
        }
    }

    // Anything not returning in the above statement means it doesn't return
    return false;
}

void DeclResolver::processExpr(Expr *&expr) {
    switch (expr->getExprKind()) {
        case Expr::Kind::AssignmentBinaryOperator:
            processAssignmentBinaryOperatorExpr(llvm::dyn_cast<AssignmentBinaryOperatorExpr>(expr));
            break;
        case Expr::Kind::BinaryOperator:
            processBinaryOperatorExpr(expr, false);
            break;
        case Expr::Kind::CharacterLiteral:
            processCharacterLiteralExpr(llvm::dyn_cast<CharacterLiteralExpr>(expr));
            break;
        case Expr::Kind::ExplicitCast:
            processExplicitCastExpr(expr);
            break;
        case Expr::Kind::FloatLiteral:
            processFloatLiteralExpr(llvm::dyn_cast<FloatLiteralExpr>(expr));
            break;
        case Expr::Kind::FunctionCall:
            processFunctionCallExpr(expr);
            break;
        case Expr::Kind::Identifier:
            processIdentifierExpr(expr);
            break;
        case Expr::Kind::ImplicitCast:
            processImplicitCastExpr(llvm::dyn_cast<ImplicitCastExpr>(expr));
            break;
        case Expr::Kind::IndexerCall:
            processIndexerCallExpr(expr);
            break;
        case Expr::Kind::IntegerLiteral:
            processIntegerLiteralExpr(llvm::dyn_cast<IntegerLiteralExpr>(expr));
            break;
        case Expr::Kind::LocalVariableDecl:
            processLocalVariableDeclExpr(llvm::dyn_cast<LocalVariableDeclExpr>(expr), false);
            break;
        case Expr::Kind::PotentialLocalVariableDecl:
            // Casting isn't required for this function. It will handle the casting for us since this is a type we will be completely removing from the AST in this function
            processLocalVariableDeclOrPrefixOperatorCallExpr(expr);
            break;
        case Expr::Kind::MemberAccessCall:
            processMemberAccessCallExpr(expr);
            break;
        case Expr::Kind::Paren:
            processParenExpr(llvm::dyn_cast<ParenExpr>(expr));
            break;
        case Expr::Kind::PostfixOperator:
            processPostfixOperatorExpr(llvm::dyn_cast<PostfixOperatorExpr>(expr));
            break;
        case Expr::Kind::PotentialExplicitCast:
            processPotentialExplicitCastExpr(expr);
            break;
        case Expr::Kind::PrefixOperator:
            processPrefixOperatorExpr(expr);
            break;
        case Expr::Kind::ResolvedTypeRef:
            processResolvedTypeRefExpr(llvm::dyn_cast<ResolvedTypeRefExpr>(expr));
            break;
        case Expr::Kind::StringLiteral:
            processStringLiteralExpr(llvm::dyn_cast<StringLiteralExpr>(expr));
            break;
        case Expr::Kind::Ternary:
            processTernaryExpr(llvm::dyn_cast<TernaryExpr>(expr));
            break;
        case Expr::Kind::UnresolvedTypeRef:
            processUnresolvedTypeRefExpr(expr);
            break;
        case Expr::Kind::InfixMacroCall:
            processInfixMacroCallExpr(expr);
            break;
        case Expr::Kind::PrefixMacroCall:
            processPrefixMacroCallExpr(expr);
            break;
    }
}

// Decls
void DeclResolver::processCopyOrMoveConstructorDecl(ConstructorDecl *constructorDecl, bool isMoveConstructor) {
    // We handle copy/move constructors differently than normal constructors
    if (constructorDecl->baseConstructorCall != nullptr) {
        if (isMoveConstructor) {
            printError(
                    "specifying a base constructor for move constructors is not allowed, base move constructor is always used!",
                    constructorDecl->baseConstructorCall->startPosition(),
                    constructorDecl->baseConstructorCall->endPosition());
        } else {
            printError(
                    "specifying a base constructor for copy constructors is not allowed, base copy constructor is always used!",
                    constructorDecl->baseConstructorCall->startPosition(),
                    constructorDecl->baseConstructorCall->endPosition());
        }
    }

    constructorDecl->baseConstructor = nullptr;

    if (currentStruct->baseStruct != nullptr) {
        if (!isMoveConstructor && currentStruct->baseStruct->copyConstructor == nullptr) {
            // If we're processing the copy constructor and the copy constructor is deleted in our base then we have
            // to delete ours as well. There is no way around this.
            printError("struct '" + currentStruct->name() + "' cannot have a copy constructor due to base '" +
                       currentStruct->baseStruct->name() + "' having a deleted copy constructor, please mark struct '" +
                       currentStruct->name() + "' [Copy(false)] to delete its copy constructor!",
                       currentStruct->startPosition(), currentStruct->endPosition());
        } else if (isMoveConstructor && currentStruct->baseStruct->moveConstructor == nullptr) {
            // If we're processing the move constructor and the move constructor is deleted in our base then we have
            // to delete ours as well
            // TODO: Couldn't we allow using the `copy` constructor of the base to allow moving again?
            printError("struct '" + currentStruct->name() + "' cannot have a move constructor due to base '" +
                       currentStruct->baseStruct->name() + "' having a deleted move constructor, please mark struct '" +
                       currentStruct->name() + "' [Move(false)] to delete its move constructor!",
                       currentStruct->startPosition(), currentStruct->endPosition());
        }

        if (isMoveConstructor) {
            // Set the base constructor to the base class move constructor
            constructorDecl->baseConstructor = currentStruct->baseStruct->moveConstructor;
        } else {
            // Set the base constructor to the base class copy constructor
            constructorDecl->baseConstructor = currentStruct->baseStruct->copyConstructor;
        }

        // Create the `other` parameter reference
        Expr* refOther = new IdentifierExpr({}, {}, "other", {});

        // Process the the ref into what it need to be
        processIdentifierExpr(refOther);

        std::vector<Expr*> arguments = {
                refOther
        };

        // Set the base constructor call
        constructorDecl->baseConstructorCall = new BaseConstructorCallExpr({}, {}, false,
                                                                           constructorDecl->baseConstructor,
                                                                           arguments);
    }

    // If it isn't user specified then we have to go through all data members and call their copy/move constructors...
    if (!constructorDecl->isUserSpecified) {
        auto& bodyStatements = constructorDecl->body()->statements();

        // NOTE: We loop in reverse to make sure they're being assigned in the correct order
        for (GlobalVariableDecl* copyOrMoveMember : gulc::reverse(currentStruct->dataMembers)) {
            if (copyOrMoveMember->name().empty()) {
                // Any member without a name is padding, we skip padding.
                continue;
            }

            auto structType = new StructType({}, {}, TypeQualifier::None,
                                             currentStruct->name(), currentStruct);

            Expr* refOther = new IdentifierExpr({}, {}, "other", {});

            // Process the the ref into what it need to be
            processIdentifierExpr(refOther);

            // Create references to both of their members
            Expr* refOtherMember = new RefStructMemberVariableExpr({}, {}, refOther,
                                                                   llvm::dyn_cast<StructType>(structType->deepCopy()),
                                                                   copyOrMoveMember);

            refOtherMember->resultType = copyOrMoveMember->type->deepCopy();

            // Delete the struct type (since we only make copies of it)
            delete structType;

            if (llvm::isa<StructType>(copyOrMoveMember->type)) {
                // If the member is a struct we will grab the struct type's copy/move constructor and call it
                auto memberStructType = llvm::dyn_cast<StructType>(copyOrMoveMember->type);

                if (!isMoveConstructor && memberStructType->decl()->copyConstructor == nullptr) {
                    // If the copy constructor of the member variable is deleted we cannot define a default copy
                    // constructor. Notify the programmer of this and how to fix it
                    printError("member '" + copyOrMoveMember->name() + "' in struct '" + structType->decl()->name() +
                               "' has a deleted copy constructor, cannot create default copy constructor for struct '" +
                               structType->decl()->name() + "', either mark the current struct as [Copy(false)] or "
                                                            "define a custom copy constructor!",
                               copyOrMoveMember->startPosition(), copyOrMoveMember->endPosition());
                } else if (isMoveConstructor && memberStructType->decl()->moveConstructor == nullptr) {
                    // If the move constructor of the member variable is deleted we cannot define a default move
                    // constructor. Notify the programmer of this and how to fix it
                    // TODO: Couldn't we just use the copy constructor here? Not everything HAS to be moved in the move
                    //       constructor, right? Or should these be guaranteed moves unless explicitly stated otherwise?
                    printError("member '" + copyOrMoveMember->name() + "' in struct '" + structType->decl()->name() +
                               "' has a deleted move constructor, cannot create default move constructor for struct '" +
                               structType->decl()->name() + "', either mark the current struct as [Move(false)] or "
                                                            "define a custom move constructor!",
                               copyOrMoveMember->startPosition(), copyOrMoveMember->endPosition());
                }

                std::vector<Expr*> arguments = {
                        refOtherMember
                };

                // Grab the proper constructor for what we're doing
                ConstructorDecl* copyOrMoveConstructor;

                if (isMoveConstructor) {
                    copyOrMoveConstructor = memberStructType->decl()->moveConstructor;
                } else {
                    copyOrMoveConstructor = memberStructType->decl()->copyConstructor;
                }

                // Call the copy constructor for the member variable
                auto constructMemberVariable = new ConstructStructMemberVariableStmt({}, {}, copyOrMoveMember,
                                                                                     copyOrMoveConstructor,
                                                                                     arguments);

                bodyStatements.insert(bodyStatements.begin(), constructMemberVariable);
            } else {
                // If the member is not a struct we will just use a value assignment from the other value
                Expr* refThis = new IdentifierExpr({}, {}, "this", {});

                processIdentifierExpr(refThis);

                Expr* refThisMember = new RefStructMemberVariableExpr({}, {}, refThis,
                                                                      llvm::dyn_cast<StructType>(structType->deepCopy()),
                                                                      copyOrMoveMember);

                refThisMember->resultType = copyOrMoveMember->type->deepCopy();

                // TODO: We don't have to dereference or anything here, right?
                //       If the `this` member is a reference the `other` member is too...
                refOtherMember->resultType->setIsLValue(true);
                convertLValueToRValue(refOtherMember);

                Expr* assignment = new AssignmentBinaryOperatorExpr({}, {}, refThisMember, refOtherMember);

                bodyStatements.insert(bodyStatements.begin(), assignment);
            }
        }
    }
}

void DeclResolver::processConstructorDecl(ConstructorDecl* constructorDecl) {
    // We back up the old values since we will be
    auto oldFunctionParams = functionParams;
    auto oldReturnType = returnType;
    auto oldFunctionLocalVariablesCount = functionLocalVariablesCount;
    auto oldFunctionLocalVariables = std::move(functionLocalVariables);
    auto oldLabelNames = std::move(labelNames);
    auto oldCurrentFunctionIsConstructor = currentFunctionIsConstructor;

    currentFunctionIsConstructor = true;

    if (constructorDecl->hasParameters()) {
        functionParams = &constructorDecl->parameters;
    } else {
        functionParams = nullptr;
    }

    returnType = nullptr;

    // We reset to zero just in case.
    functionLocalVariablesCount = 0;

    bool isMoveConstructor = constructorDecl == currentStruct->moveConstructor;
    bool isCopyConstructor = constructorDecl == currentStruct->copyConstructor;

    if (isCopyConstructor) {
        processCopyOrMoveConstructorDecl(constructorDecl, false);
    } else if (isMoveConstructor) {
        processCopyOrMoveConstructorDecl(constructorDecl, true);
    } else if (constructorDecl->baseConstructorCall != nullptr) {
        // If the call isn't null here it means it was specified in code, we will have to search for a valid base
        // constructor

        // If the arguments are empty and it isn't a `this` call we can just use the default base constructor we
        // already have
        if (!constructorDecl->baseConstructorCall->isThisCall() &&
            !constructorDecl->baseConstructorCall->hasArguments()) {
            // Delete the old base constructor call
            delete constructorDecl->baseConstructorCall;

            // NOTE: We don't set the `this` argument here. That will be handled by the code generator
            constructorDecl->baseConstructorCall = new BaseConstructorCallExpr({}, {},
                                                                               false,
                                                                               constructorDecl->baseConstructor, {});
        } else {
            // First we have to process the arguments to make sure they're valid...
            for (Expr *&argument : constructorDecl->baseConstructorCall->arguments) {
                processExpr(argument);
            }

            StructType* structType = nullptr;

            if (constructorDecl->baseConstructorCall->isThisCall()) {
                structType = new StructType({}, {}, TypeQualifier::None,
                                            constructorDecl->parentStruct->name(), constructorDecl->parentStruct);
            } else {
                structType = new StructType({}, {}, TypeQualifier::None,
                                            constructorDecl->parentStruct->baseStruct->name(),
                                            constructorDecl->parentStruct->baseStruct);
            }

            ConstructorDecl *foundConstructor = nullptr;
            // `isExactMatch` is used to check for ambiguity
            bool isExactMatch = false;
            bool isAmbiguous = false;

            for (ConstructorDecl *checkConstructorDecl : structType->decl()->constructors) {
                // Skip any constructors not visible to us
                if (!VisibilityChecker::canAccessStructMember(structType, currentStruct,
                                                              checkConstructorDecl)) {
                    continue;
                }

                if (!checkConstructorOrFunctionMatchesCall(foundConstructor, checkConstructorDecl,
                                                           &constructorDecl->baseConstructorCall->arguments,
                                                           &isExactMatch, &isAmbiguous)) {
                    printError("base constructor call is ambiguous!",
                               constructorDecl->baseConstructorCall->startPosition(),
                               constructorDecl->baseConstructorCall->endPosition());
                }
            }

            // Delete the temporary struct type;
            delete structType;

            if (foundConstructor) {
                if (isAmbiguous) {
                    printError("base constructor call is ambiguous!",
                               constructorDecl->baseConstructorCall->startPosition(),
                               constructorDecl->baseConstructorCall->endPosition());
                }

                if (!isExactMatch) {
                    // TODO: Implicitly cast and extract default values from the constructor parameter list
                }

                currentFileAst->addImportExtern(foundConstructor);
                constructorDecl->baseConstructor = foundConstructor;
                constructorDecl->baseConstructorCall->baseConstructor = foundConstructor;

                // Since we found a found a constructor we have to convert any lvalues to rvalues and
                // dereference references where it makes sense...
                for (std::size_t i = 0; i < foundConstructor->parameters.size(); ++i) {
                    if (!getTypeIsReference(foundConstructor->parameters[i]->type)) {
                        dereferenceReferences(constructorDecl->baseConstructorCall->arguments[i]);
                        convertLValueToRValue(constructorDecl->baseConstructorCall->arguments[i]);
                    }
                }
            } else {
                printError("base struct type '" + constructorDecl->parentStruct->baseStruct->name() +
                           "' does not have a visible constructor matching the specified base constructor call!",
                           constructorDecl->baseConstructorCall->startPosition(),
                           constructorDecl->baseConstructorCall->endPosition());
            }
        }
    } else if (constructorDecl->baseConstructor != nullptr) {
        // If it reaches this point it means there was no base constructor specified in the source code, so we
        // implicitly call the base's default constructor

        // NOTE: We don't set the `this` argument here. That will be handled by the code generator
        constructorDecl->baseConstructorCall = new BaseConstructorCallExpr({}, {},
                                                                           false,
                                                                           constructorDecl->baseConstructor, {});
    }

    // Clear the list of member assignments since we need to know if they've been assigned in this constructor
    constructorAssignedMember.clear();

    processCompoundStmt(constructorDecl->body(), true);

    // TODO: We need to go through the constructor body and detect any assignments. If an assignment is detected, then
    //       we shouldn't implicitly construct the variable below. We will also need to detect when we don't implicitly
    //       construct variables and remove any destructor calls that will be placed once we add `move` and `copy`
    //       constructors...
    // If the base constructor is null or isn't a `this` call...
    // We also don't perform this operation when the constructor is a move or copy constructor
    if (!isMoveConstructor && !isCopyConstructor && (constructorDecl->baseConstructor == nullptr ||
            constructorDecl->baseConstructor->parentStruct != constructorDecl->parentStruct)) {
        // Construct any members or error if a member doesn't have a default constructor
        // NOTE: We loop backwards so the variables are constructed in the body in the order they're declared
        //       (since we always insert at the beginning we have to loop backwards or they will be backwards)
        // TODO: We need to detect if a member variable is assigned in the constructor body
        for (GlobalVariableDecl *dataMember : gulc::reverse(constructorDecl->parentStruct->dataMembers)) {
            if (llvm::isa<StructType>(dataMember->type)) {
                if (std::find(constructorAssignedMember.begin(), constructorAssignedMember.end(),
                              dataMember) == constructorAssignedMember.end()) {
                    // We skip any member that has already been constructed in the constructor body...
                    continue;
                }

                auto structType = llvm::dyn_cast<StructType>(dataMember->type);
                auto structDecl = structType->decl();

                ConstructorDecl *foundConstructor = nullptr;

                // Find the default constructor
                for (ConstructorDecl *checkConstructor : structDecl->constructors) {
                    if (checkConstructor->parameters.empty()) {
                        // Skip any constructors not visible to us
                        if (!VisibilityChecker::canAccessStructMember(structType, currentStruct,
                                                                      checkConstructor)) {
                            continue;
                        } else {
                            foundConstructor = checkConstructor;
                            break;
                        }
                    }
                }

                if (!foundConstructor) {
                    printError(
                            "struct '" + structDecl->name() + "' does not have a publicly visible default constructor!",
                            dataMember->startPosition(), dataMember->endPosition());
                }

                auto constructStructMemberVariable = new ConstructStructMemberVariableStmt({}, {},
                                                                                           dataMember,
                                                                                           foundConstructor,
                                                                                           {});

                std::vector<Stmt *> &bodyStmts = constructorDecl->body()->statements();
                bodyStmts.insert(bodyStmts.begin(), constructStructMemberVariable);
            }
        }
    }

    currentFunctionIsConstructor = oldCurrentFunctionIsConstructor;
    labelNames = std::move(oldLabelNames);
    functionLocalVariablesCount = oldFunctionLocalVariablesCount;
    functionLocalVariables = std::move(oldFunctionLocalVariables);
    returnType = oldReturnType;
    functionParams = oldFunctionParams;
}

void DeclResolver::processDestructorDecl(DestructorDecl *destructorDecl) {
    // We back up the old values since we will be
    auto oldFunctionParams = functionParams;
    auto oldReturnType = returnType;
    auto oldFunctionLocalVariablesCount = functionLocalVariablesCount;
    auto oldFunctionLocalVariables = std::move(functionLocalVariables);
    auto oldLabelNames = std::move(labelNames);

    functionParams = nullptr;

    returnType = nullptr;

    // We reset to zero just in case.
    functionLocalVariablesCount = 0;

    processCompoundStmt(destructorDecl->body(), true);

    labelNames = std::move(oldLabelNames);
    functionLocalVariablesCount = oldFunctionLocalVariablesCount;
    functionLocalVariables = std::move(oldFunctionLocalVariables);
    returnType = oldReturnType;
    functionParams = oldFunctionParams;
}

void DeclResolver::processEnumDecl(EnumDecl *enumDecl) {
    if (enumDecl->hasConstants()) {
        for (EnumConstantDecl* enumConstant : enumDecl->enumConstants()) {
            if (enumConstant->hasConstantValue()) {
                enumConstant->constantValue = solveConstExpression(enumConstant->constantValue);
            } else {
                printError("enum constants MUST have a constant value, uninitialized enum constants are not yet supported!",
                           enumConstant->startPosition(), enumConstant->endPosition());
                return;
            }
        }
    }
}

void DeclResolver::processFunctionDecl(FunctionDecl *functionDecl) {
    for (ParameterDecl* parameter : functionDecl->parameters) {
        // Convert any template type references to the supplied type
        parameter->typeTemplateParamNumber = applyTemplateTypeArguments(parameter->type);
    }

    if (functionDecl->resultType->qualifier() == TypeQualifier::Const) {
        printWarning("`const` qualifier unneeded on function result type, function return types are already `const` qualified!",
                     functionDecl->startPosition(), functionDecl->endPosition());
    } else if (functionDecl->resultType->qualifier() == TypeQualifier::Mut) {
        printWarning("`mut` does not apply to function return types, `mut` qualifier is ignored...",
                     functionDecl->startPosition(), functionDecl->endPosition());
    }
    // The result type CAN be `immut` since `immut` has a requirement of NEVER being modified -
    //  unlike `const` which can be modified in situations where the underlying type has a `mut` member variable...

    // Convert any template type references to the supplied type
    applyTemplateTypeArguments(functionDecl->resultType);

    // We back up the old values since we will be
    auto oldFunctionParams = functionParams;
    auto oldReturnType = returnType;
    auto oldFunctionLocalVariablesCount = functionLocalVariablesCount;
    auto oldFunctionLocalVariables = std::move(functionLocalVariables);
    auto oldLabelNames = std::move(labelNames);

    if (functionDecl->hasParameters()) {
        functionParams = &functionDecl->parameters;
    }

    returnType = functionDecl->resultType;

    // We reset to zero just in case.
    functionLocalVariablesCount = 0;

    currentFunction = functionDecl;

    processCompoundStmt(functionDecl->body(), true);

    currentFunction = nullptr;

    labelNames = std::move(oldLabelNames);
    functionLocalVariablesCount = oldFunctionLocalVariablesCount;
    functionLocalVariables = std::move(oldFunctionLocalVariables);
    returnType = oldReturnType;
    functionParams = oldFunctionParams;
}

Expr *DeclResolver::solveConstExpression(Expr *expr) {
    switch (expr->getExprKind()) {
        case Expr::Kind::CharacterLiteral:
        case Expr::Kind::FloatLiteral:
        case Expr::Kind::IntegerLiteral:
            return expr;
        case Expr::Kind::Paren: {
            // For paren we delete the old `ParenExpr` before passing the contained expression to the solver
            auto paren = llvm::dyn_cast<ParenExpr>(expr);
            Expr* contained = paren->containedExpr;
            paren->containedExpr = nullptr;
            delete paren;
            return solveConstExpression(contained);
        }
        case Expr::Kind::BinaryOperator: {
            std::cerr << "gulc error: binary operators not yet supported for `constexpr`!" << std::endl;
            std::exit(1);
            return nullptr;
        }
        default:
            std::cerr << "gulc error: unsupported expression, expected `constexpr`!" << std::endl;
            std::exit(1);
            return nullptr;
    }
    return nullptr;
}

void DeclResolver::processGlobalVariableDecl(GlobalVariableDecl *globalVariableDecl) {
    if (globalVariableDecl->hasInitialValue()) {
        processExpr(globalVariableDecl->initialValue);
    }
}

void DeclResolver::processNamespaceDecl(NamespaceDecl *namespaceDecl) {
    NamespaceDecl* oldNamespace = currentNamespace;
    currentNamespace = namespaceDecl;

    for (Decl* decl : namespaceDecl->nestedDecls()) {
        processDecl(decl);
    }

    currentNamespace = oldNamespace;
}

void DeclResolver::processStructDecl(StructDecl *structDecl) {
    StructDecl* oldStruct = currentStruct;
    currentStruct = structDecl;

    bool processBaseStruct = structDecl->baseStruct != nullptr;
    bool baseHasVisibleDefaultConstructor = false;
    ConstructorDecl* foundBaseDefaultConstructor = nullptr;

    if (processBaseStruct) {
        for (ConstructorDecl* constructor : structDecl->baseStruct->constructors) {
            Decl::Visibility constructorVisibility = constructor->visibility();

            // TODO: Once we support accessing external libraries we will have to support checking if we can access
            //  the constructor differently
            if (constructor->parameters.empty() &&
                (constructorVisibility != Decl::Visibility::Private)) {
                baseHasVisibleDefaultConstructor = true;
                foundBaseDefaultConstructor = constructor;
                break;
            }
        }

        if (!baseHasVisibleDefaultConstructor) {
            printError("base struct default constructor '" + structDecl->baseStruct->name() + "." +
                       structDecl->baseStruct->name() + "()' is inaccessible due to its protection level!",
                       structDecl->startPosition(), structDecl->endPosition());
        }

        // Make sure we can actually call the base constructor by making a prototype for it in the output
        currentFileAst->addImportExtern(foundBaseDefaultConstructor);
    }

    for (ConstructorDecl* constructor : structDecl->constructors) {
        // We HAVE to add the base constructor call BEFORE processing the constructor so that `processConstructorDecl`
        // can handle creating a proper call to the base constructor (i.e. creating implicit casts, etc.)
        if (processBaseStruct) {
            // NOTE: Here we are only setting the default base constructor. If `constructor->baseConstructorCall`
            //       isn't null here then this will be replaced within `processConstructorDecl`
            constructor->baseConstructor = foundBaseDefaultConstructor;
        }

        processConstructorDecl(constructor);
    }

    for (Decl* decl : structDecl->members) {
        processDecl(decl);
    }

    // At this point the destructor is set so if there is a base type we have to set it to call the base destructor
    if (processBaseStruct) {
        // Make sure we can actually call the base struct's destructor by making a prototype for it
        currentFileAst->addImportExtern(structDecl->baseStruct->destructor);

        structDecl->destructor->baseDestructor = structDecl->baseStruct->destructor;
    }

    // Finally, process the destructor...
    processDestructorDecl(structDecl->destructor);

    currentStruct = oldStruct;
}

void DeclResolver::processTemplateFunctionDecl(TemplateFunctionDecl *templateFunctionDecl) {
    if (templateFunctionDecl->hasTemplateParameters() && !templateFunctionDecl->parameters.empty()) {
        for (TemplateParameterDecl* templateParameter : templateFunctionDecl->templateParameters) {
            for (ParameterDecl* parameter : templateFunctionDecl->parameters) {
                if (templateParameter->name() == parameter->name()) {
                    printError("parameter `" + parameter->name() + "` shadows template parameter of the same name!",
                               parameter->startPosition(), parameter->endPosition());
                    return;
                }
            }
        }
    }
}

void DeclResolver::processTemplateFunctionDeclImplementation(TemplateFunctionDecl *templateFunctionDecl,
                                                             std::vector<Expr*>& templateArgs,
                                                             FunctionDecl *implementedFunction) {
    auto oldCurrentNamespace = currentNamespace;
    currentNamespace = templateFunctionDecl->parentNamespace;
    auto oldFunctionTemplateParams = functionTemplateParams;
    functionTemplateParams = &templateFunctionDecl->templateParameters;
    auto oldFunctionTemplateArgs = functionTemplateArgs;
    functionTemplateArgs = &templateArgs;

    processFunctionDecl(implementedFunction);

    functionTemplateArgs = oldFunctionTemplateArgs;
    functionTemplateParams = oldFunctionTemplateParams;
    currentNamespace = oldCurrentNamespace;
}

// Stmts
bool DeclResolver::processBreakStmt(BreakStmt *breakStmt) {
    if (!breakStmt->label().empty()) {
        addUnresolvedLabel(breakStmt->label());
    }

    // NOTE: This is an iffy scenario. We need a way to say "all code after this is unreachable" but not that there is
    // a return
    return false;
}

bool DeclResolver::processCaseStmt(CaseStmt *caseStmt) {
    if (caseStmt->hasCondition()) {
        processExpr(caseStmt->condition);
    }

    return processStmt(caseStmt->trueStmt);
}

bool DeclResolver::processCompoundStmt(CompoundStmt *compoundStmt, bool isFunctionBody) {
    bool returnsOnAllCodePaths = false;

    unsigned int oldLocalVariableCount = functionLocalVariablesCount;

    for (Stmt*& stmt : compoundStmt->statements()) {
        if (processStmt(stmt)) {
            returnsOnAllCodePaths = true;
        }
    }

    functionLocalVariablesCount = oldLocalVariableCount;

    if (isFunctionBody && !returnsOnAllCodePaths) {
        // If `returnType` is null that means we're in either a constructor or a destructor
        if (returnType == nullptr) {
            // Add in a default return at the end of the body
            compoundStmt->statements().push_back(new ReturnStmt({}, {}, nullptr));
        } else {
            if (llvm::isa<BuiltInType>(returnType)) {
                auto builtInType = llvm::dyn_cast<BuiltInType>(returnType);

                if (builtInType->size() == 0) {
                    // Add in a default return at the end of the body
                    compoundStmt->statements().push_back(new ReturnStmt({}, {}, nullptr));
                } else {
                    // We can safely reference 'currentFunction' since both `ConstructorDecl` and 'DestructorDecl`
                    // return void
                    printError("function '" + currentFunction->name() + "' does not return on all code paths!",
                               currentFunction->startPosition(), currentFunction->endPosition());
                }
            } else {
                // We can safely reference 'currentFunction' since both `ConstructorDecl` and 'DestructorDecl`
                // return void
                printError("function '" + currentFunction->name() + "' does not return on all code paths!",
                           currentFunction->startPosition(), currentFunction->endPosition());
            }
        }
    }

    return returnsOnAllCodePaths;
}

bool DeclResolver::processContinueStmt(ContinueStmt *continueStmt) {
    if (!continueStmt->label().empty()) {
        addUnresolvedLabel(continueStmt->label());
    }

    // NOTE: This is an iffy scenario. We need a way to say "all code after this is unreachable" but not that there is
    // a return
    return false;
}

bool DeclResolver::processDoStmt(DoStmt *doStmt) {
    // If the loop statement is null then we can't return on all code paths
    bool returnsOnAllCodePaths = doStmt->loopStmt != nullptr;

    if (doStmt->loopStmt != nullptr) {
        if (!processStmt(doStmt->loopStmt)) {
            returnsOnAllCodePaths = false;
        }
    }

    processExpr(doStmt->condition);

    return returnsOnAllCodePaths;
}

bool DeclResolver::processForStmt(ForStmt *forStmt) {
    // If the loop statement is null then we can't return on all code paths
    bool returnsOnAllCodePaths = forStmt->loopStmt != nullptr;

    // Since preloop can declare variables we have to back up and restore the old variables
    // That way the `i` from `for (int i;;);` won't be accessible outside of the for loop
    unsigned int oldLocalVariableCount = functionLocalVariablesCount;

    if (forStmt->preLoop != nullptr) processExpr(forStmt->preLoop);
    if (forStmt->condition != nullptr) processExpr(forStmt->condition);
    if (forStmt->iterationExpr != nullptr) processExpr(forStmt->iterationExpr);

    if (forStmt->loopStmt != nullptr) {
        if (!processStmt(forStmt->loopStmt)) {
            returnsOnAllCodePaths = false;
        }
    }

    functionLocalVariablesCount = oldLocalVariableCount;

    return returnsOnAllCodePaths;
}

bool DeclResolver::processGotoStmt(GotoStmt *gotoStmt) {
    if (!gotoStmt->label.empty()) {
        addUnresolvedLabel(gotoStmt->label);
    }

    return false;
}

bool DeclResolver::processIfStmt(IfStmt *ifStmt) {
    // If there isn't a statement for the `true` part of the if statement then we cannot return on all code paths
    // UNLESS we can evaluate the if statement to always be false and the `false` part isn't null
    bool returnsOnAllCodePaths = ifStmt->trueStmt != nullptr;

    processExpr(ifStmt->condition);

    if (ifStmt->trueStmt != nullptr) {
        if (!processStmt(ifStmt->trueStmt)) {
            returnsOnAllCodePaths = false;
        }
    }

    if (ifStmt->hasFalseStmt()) {
        if (!processStmt(ifStmt->falseStmt)) {
            returnsOnAllCodePaths = false;
        }
    } else {
        // If there isn't an `else` statement then this can't be detected as returning on all code paths unless we
        // evaluate the condition to always be true...
        returnsOnAllCodePaths = false;
    }

    return returnsOnAllCodePaths;
}

bool DeclResolver::processLabeledStmt(LabeledStmt *labeledStmt) {
    // Store the number of local variables that were declared before us
    labeledStmt->currentNumLocalVariables = functionLocalVariablesCount;

    currentFunction->labeledStmts.insert({labeledStmt->label(), labeledStmt});

    labelResolved(labeledStmt->label());

    return processStmt(labeledStmt->labeledStmt);
}

bool DeclResolver::processReturnStmt(ReturnStmt *returnStmt) {
    if (returnStmt->hasReturnValue()) {
        processExpr(returnStmt->returnValue);

        bool typesAreSame;

        // If the return value isn't a reference but the return type of the function is we need to create a reference? This should probably be illegal?
        if (!getTypeIsReference(returnStmt->returnValue->resultType)) {
            if (llvm::isa<ReferenceType>(returnType)) {
                auto referenceType = llvm::dyn_cast<ReferenceType>(returnType);
                typesAreSame = TypeComparer::getTypesAreSame(returnStmt->returnValue->resultType, referenceType->referenceToType, true);
            } else {
                typesAreSame = TypeComparer::getTypesAreSame(returnStmt->returnValue->resultType, returnType, true);
            }
        // If the return type of the function isn't a reference but the result value type is then we dereference the result value
        } else if (!getTypeIsReference(returnType)) {
            if (llvm::isa<ReferenceType>(returnStmt->returnValue->resultType)) {
                auto referenceType = llvm::dyn_cast<ReferenceType>(returnStmt->returnValue->resultType);
                typesAreSame = TypeComparer::getTypesAreSame(returnType, referenceType->referenceToType, true);
            } else {
                typesAreSame = TypeComparer::getTypesAreSame(returnStmt->returnValue->resultType, returnType, true);
            }
        } else {
            typesAreSame = TypeComparer::getTypesAreSame(returnStmt->returnValue->resultType, returnType, true);
        }

        bool typeIsReference = getTypeIsReference(returnType);

        if (!typeIsReference) {
            dereferenceReferences(returnStmt->returnValue);
        }

        if (!typesAreSame) {
            returnStmt->returnValue = applyCast(returnStmt->returnValue->resultType, returnType,
                                                returnStmt->returnValue,
                                                CastOperatorType::Implicit);
        }

        if (!typeIsReference) {
            convertLValueToRValue(returnStmt->returnValue);
        }
    }

    return true;
}

bool DeclResolver::processSwitchStmt(SwitchStmt *switchStmt) {
    bool returnsOnAllCodePaths = !switchStmt->cases().empty();

    // TODO: Should we add the type of 'SwitchStmt::condition' to the context?
    processExpr(switchStmt->condition);

    for (CaseStmt* caseStmt : switchStmt->cases()) {
        if (!processCaseStmt(caseStmt)) {
            returnsOnAllCodePaths = false;
        }
    }

    return returnsOnAllCodePaths;
}

bool DeclResolver::processTryStmt(TryStmt *tryStmt) {
    bool returnsOnAllCodePaths = true;

    if (!processCompoundStmt(tryStmt->encapsulatedStmt, false)) {
        returnsOnAllCodePaths = false;
    }

    if (tryStmt->hasCatchStmts()) {
        for (TryCatchStmt*& catchStmt : tryStmt->catchStmts()) {
            if (!processTryCatchStmt(catchStmt)) {
                returnsOnAllCodePaths = false;
            }
        }
    }

    if (tryStmt->hasFinallyStmt()) {
        if (!processTryFinallyStmt(tryStmt->finallyStmt)) {
            returnsOnAllCodePaths = false;
        }
    }

    return returnsOnAllCodePaths;
}

bool DeclResolver::processTryCatchStmt(TryCatchStmt *tryCatchStmt) {
    return processCompoundStmt(tryCatchStmt->handlerStmt, false);
}

bool DeclResolver::processTryFinallyStmt(TryFinallyStmt *tryFinallyStmt) {
    return processCompoundStmt(tryFinallyStmt->handlerStmt, false);
}

bool DeclResolver::processWhileStmt(WhileStmt *whileStmt) {
    processExpr(whileStmt->condition);
    if (whileStmt->loopStmt) return processStmt(whileStmt->loopStmt);

    return false;
}

// Exprs
OperatorDecl *DeclResolver::findInfixOperatorOrError(StructDecl *structDecl, std::vector<Expr *> *args,
                                                     std::string const& operatorName,
                                                     TextPosition const& startPosition,
                                                     TextPosition const& endPosition) {
    OperatorDecl* foundOperator = nullptr;
    bool isExactMatch = false;
    bool isAmbiguous = false;

    // First we search the struct's direct members...
    for (Decl* checkDecl : structDecl->members) {
        if (llvm::isa<OperatorDecl>(checkDecl)) {
            auto operatorDecl = llvm::dyn_cast<OperatorDecl>(checkDecl);

            // If the checkDecl is an operator we need to make sure it is a binary operator...
            if (operatorDecl->operatorType() == OperatorType::Infix) {
                if (operatorDecl->operatorName() == operatorName) {
                    if (!checkConstructorOrFunctionMatchesCall(foundOperator, operatorDecl,
                                                               args, &isExactMatch, &isAmbiguous)) {
                        printError("operator `" + operatorName + "` is ambiguous for the provided types!",
                                   startPosition, endPosition);
                    }
                }
            }
        }
    }

    // Then we search the struct's inherited members if we didn't find an exact match...
    if (!isExactMatch) {
        for (Decl* checkDecl : structDecl->inheritedMembers) {
            if (llvm::isa<OperatorDecl>(checkDecl)) {
                auto operatorDecl = llvm::dyn_cast<OperatorDecl>(checkDecl);

                // If the checkDecl is an operator we need to make sure it is a binary operator...
                if (operatorDecl->operatorType() == OperatorType::Infix) {
                    if (foundOperator->operatorName() == operatorName) {
                        if (!checkConstructorOrFunctionMatchesCall(foundOperator, operatorDecl,
                                                                   args, &isExactMatch, &isAmbiguous)) {
                            printError("operator `" + operatorName + "` is ambiguous for the provided types!",
                                       startPosition, endPosition);
                        }
                    }
                }
            }
        }
    }

    if (!foundOperator) {
        printError("operator `" + operatorName + "` does not exist for the provided types!",
                   startPosition, endPosition);
    }

    if (isAmbiguous) {
        printError("operator `" + operatorName + "` is ambiguous for the provided types!",
                   startPosition, endPosition);
    }

    return foundOperator;
}

OperatorDecl *DeclResolver::findPrefixOperator(StructDecl *structDecl, std::string const &operatorName) {
    OperatorDecl* foundOperator = nullptr;

    for (Decl* checkMember : structDecl->members) {
        if (llvm::isa<OperatorDecl>(checkMember)) {
            auto checkOperator = llvm::dyn_cast<OperatorDecl>(checkMember);

            if (checkOperator->operatorType() == OperatorType::Prefix) {
                if (checkOperator->operatorName() == operatorName) {
                    // TODO: Once we allow `const` function modifiers we need to check for the const modifier on the
                    //       function
                    foundOperator = checkOperator;
                    break;
                }
            }
        }
    }

    // TODO: Should we allow inheriting prefix operators? I haven't fully decided, it doesn't make very much sense
    if (!foundOperator) {
        for (Decl *checkMember : structDecl->inheritedMembers) {
            if (llvm::isa<OperatorDecl>(checkMember)) {
                auto checkOperator = llvm::dyn_cast<OperatorDecl>(checkMember);

                if (checkOperator->operatorType() == OperatorType::Prefix) {
                    if (checkOperator->operatorName() == operatorName) {
                        // TODO: Once we allow `const` function modifiers we need to check for the const modifier on the
                        //       function
                        foundOperator = checkOperator;
                        break;
                    }
                }
            }
        }
    }

    return foundOperator;
}

void DeclResolver::processAssignmentBinaryOperatorExpr(AssignmentBinaryOperatorExpr *assignmentBinaryOperatorExpr) {
    processExpr(assignmentBinaryOperatorExpr->leftValue);
    processExpr(assignmentBinaryOperatorExpr->rightValue);

    Type* leftType = assignmentBinaryOperatorExpr->leftValue->resultType;
    Type* rightType = assignmentBinaryOperatorExpr->rightValue->resultType;

    if (!assignmentBinaryOperatorExpr->hasNestedOperator()) {
        bool convertRightValueToRValue = false;

        // If there isn't a nested operator then we (i.e. no `+=`, `/=`, etc.) then we convert lvalues to rvalues
        // and dereference any references. This is a normal assignment
        if (getTypeIsReference(leftType)) {
            // If the left type is a reference and the left value is a local variable declaration then we set the
            // left variable to the reference of what is on the right...
            // If the left value ISN'T a local variable then we deref the left value and set it equal to the right
            // value (with the right value being converted to an rvalue)
            if (llvm::isa<LocalVariableDeclExpr>(assignmentBinaryOperatorExpr->leftValue)) {
                // Right value must be either lvalue or reference
                if (!(rightType->isLValue() || getTypeIsReference(rightType))) {
                    printError("initial value for reference variable MUST be an lvalue!",
                               assignmentBinaryOperatorExpr->rightValue->startPosition(),
                               assignmentBinaryOperatorExpr->rightValue->endPosition());
                    return;
                }

                // If the right type isn't already a reference then make it one...
                if (!getTypeIsReference(rightType)) {
                    assignmentBinaryOperatorExpr->rightValue = new PrefixOperatorExpr(
                            assignmentBinaryOperatorExpr->rightValue->startPosition(),
                            assignmentBinaryOperatorExpr->rightValue->endPosition(),
                            ".ref", assignmentBinaryOperatorExpr->rightValue);
                    processExpr(assignmentBinaryOperatorExpr->rightValue);
                    rightType = assignmentBinaryOperatorExpr->rightValue->resultType;
                }
            } else {
                // This will dereference the left value
                dereferenceReferences(assignmentBinaryOperatorExpr->leftValue);
                leftType = assignmentBinaryOperatorExpr->leftValue->resultType;

                // The right value here MUST be an rvalue...
                convertRightValueToRValue = true;
                rightType = assignmentBinaryOperatorExpr->rightValue->resultType;
            }
        } else {
            // If the left value isn't a reference then the right value has to be an rvalue...
            convertRightValueToRValue = true;
            rightType = assignmentBinaryOperatorExpr->rightValue->resultType;
        }

        if (!TypeComparer::getTypesAreSame(leftType, rightType, true)) {
            assignmentBinaryOperatorExpr->rightValue = applyCast(rightType, leftType,
                                                                 assignmentBinaryOperatorExpr->rightValue,
                                                                 CastOperatorType::Implicit);
        }

        if (convertRightValueToRValue) {
            convertLValueToRValue(assignmentBinaryOperatorExpr->rightValue);
        }
    } else {
        // TODO: When we support implementing custom casting rules we need to check for ambiguity.
        //        * If we have `a + b` and `a` has an `operator infix +` that accepts the type for `b` there is NO implicit cast
        //        * If we have `a + b` and there is no operator that accepts `b` we have to check if we can implicit cast `b` to `a` then `a` to `b`
        //        * If we have `a + b` and the types both have rules allowing cross implicit casting this is an ambiguity (fix by adding explicit cast)

        // If there IS a nested operator we have to figure out if it is overloaded or not and handle it appropriately.
        if (llvm::isa<BuiltInType>(leftType)) {
            if (llvm::isa<BuiltInType>(rightType)) {
                // NOTE: For operators like `+=`, `/=`, etc. the right side is ALWAYS casted to the left side type
                //       the only time this differs is if the operator being used is overloaded.
                if (!TypeComparer::getTypesAreSame(leftType, rightType, true)) {
                    // Cast the right value to the left value's type
                    assignmentBinaryOperatorExpr->rightValue = applyCast(rightType, leftType,
                                                                         assignmentBinaryOperatorExpr->rightValue,
                                                                         CastOperatorType::Implicit);
                }
            } else {
                printError("built in type `" + leftType->getString() + "` currently only supports built in operators!",
                           assignmentBinaryOperatorExpr->startPosition(), assignmentBinaryOperatorExpr->endPosition());
            }
        } else if (llvm::isa<StructType>(leftType)) {
            auto structType = llvm::dyn_cast<StructType>(leftType);
            auto structDecl = structType->decl();

            std::vector<Expr*> operatorArgs {
                    assignmentBinaryOperatorExpr->rightValue
            };

            OperatorDecl* foundOperator = findInfixOperatorOrError(structDecl, &operatorArgs,
                                                                   assignmentBinaryOperatorExpr->nestedOperator(),
                                                                   assignmentBinaryOperatorExpr->startPosition(),
                                                                   assignmentBinaryOperatorExpr->endPosition());

            // TODO: We need to check if `rightValue` needs referenced, dereferenced, or implicitly casted...

            // TODO: We need to check if `foundOperator->resultType` is equal to `leftValue->resultType`
            if (!TypeComparer::getTypesAreSame(assignmentBinaryOperatorExpr->leftValue->resultType,
                                              foundOperator->resultType, true)) {
                // TODO: At some point we should support implicit casting here.
                printError("operator `" + assignmentBinaryOperatorExpr->nestedOperator() +
                           "` does not return required type '" + structDecl->name() + "'!",
                           assignmentBinaryOperatorExpr->startPosition(),
                           assignmentBinaryOperatorExpr->endPosition());
            }

            // If the found operator is in a namespace we need to extern it (as we assume it could be in another
            // file or not declared yet)
            if (foundOperator->parentNamespace != nullptr) {
                currentFileAst->addImportExtern(foundOperator);
            }

            assignmentBinaryOperatorExpr->nestedOperatorOverload = foundOperator;
        } else {
            printError("operator `" + assignmentBinaryOperatorExpr->nestedOperator() + "` used on unsupported type '" +
                       assignmentBinaryOperatorExpr->leftValue->resultType->getString() + "'!",
                       assignmentBinaryOperatorExpr->startPosition(), assignmentBinaryOperatorExpr->endPosition());
        }
    }

    // TODO: If we're in a constructor we need to detect member sets and notify the constructor of them being set

    // The result of the assignment is always the type of the left value
    assignmentBinaryOperatorExpr->resultType = assignmentBinaryOperatorExpr->leftValue->resultType->deepCopy();
}

void DeclResolver::processBinaryOperatorExpr(Expr*& expr, bool isNestedBinaryOperator) {
    auto binaryOperatorExpr = llvm::dyn_cast<BinaryOperatorExpr>(expr);

    // TODO: We need to make it possible to set that a local variable has an initial value...
    if (llvm::isa<ResolvedTypeRefExpr>(binaryOperatorExpr->leftValue)) {
        // If the left value of the binary operator is a resolved type then this is an undetected local variable decl
        if (!llvm::isa<IdentifierExpr>(binaryOperatorExpr->rightValue)) {
            printError("expected variable name after type!",
                       binaryOperatorExpr->startPosition(), binaryOperatorExpr->endPosition());
        }

        // We process the binary operator because types can hold expressions that need resolved in their template
        // arguments list.
        processExpr(binaryOperatorExpr->leftValue);
        // NOTE: The right value is NOT processed as we only care about the string it holds.

        // NOTE: We will be stealing this pointer
        auto localVarTypeRef = llvm::dyn_cast<ResolvedTypeRefExpr>(binaryOperatorExpr->leftValue);
        auto varNameExpr = llvm::dyn_cast<IdentifierExpr>(binaryOperatorExpr->rightValue);

        if (varNameExpr->hasTemplateArguments()) {
            printError("local variable name cannot have template arguments!",
                       varNameExpr->startPosition(),
                       varNameExpr->endPosition());
            return;
        }

        // We will steal the left value pointer for our own uses...
        binaryOperatorExpr->leftValue = nullptr;

        // TODO: Support custom type suffixes
        if (binaryOperatorExpr->operatorName() == "*") {
            // Changed the resolved type to be a pointer since we now know it is a pointer...
            localVarTypeRef->resolvedType = new PointerType(localVarTypeRef->startPosition(),
                                                            localVarTypeRef->endPosition(),
                                                            TypeQualifier::None,
                                                            localVarTypeRef->resolvedType);
        } else if (binaryOperatorExpr->operatorName() == "&") {
            // Change the resolved type to be a reference
            localVarTypeRef->resolvedType = new ReferenceType(localVarTypeRef->startPosition(),
                                                              localVarTypeRef->endPosition(),
                                                              TypeQualifier::None,
                                                              localVarTypeRef->resolvedType);
        } else {
            printError("unknown type suffix `" + binaryOperatorExpr->operatorName() + "`!",
                       binaryOperatorExpr->startPosition(), binaryOperatorExpr->endPosition());
        }

        // Create the new local variable declaration that replaces the binary operator in the AST
        auto localVarDeclExpr = new LocalVariableDeclExpr(binaryOperatorExpr->startPosition(),
                                                          binaryOperatorExpr->endPosition(), localVarTypeRef,
                                                          varNameExpr->name());

        // We process the local variable normally so it is in the local variables list...
        processLocalVariableDeclExpr(localVarDeclExpr, false);

        // Delete the binary operator expression (this will delete the left and right expressions if they're not null)
        delete binaryOperatorExpr;
        expr = localVarDeclExpr;

        // No futher processing is possible, we delete the old binary operator
        return;
    } else {
        // If we reach this point we know the following are true:
        // 1. The left value is NOT a type reference
        // 2. The operator is NOT an assignment (these are changed in `TypeResolver`)
        //
        // So now we process the binary operator as a normal binary operator...
        processExpr(binaryOperatorExpr->leftValue);
        processExpr(binaryOperatorExpr->rightValue);

        // We create temporary variables just to make things easier.
        auto leftType = binaryOperatorExpr->leftValue->resultType;
        auto rightType = binaryOperatorExpr->rightValue->resultType;

        if (llvm::isa<BuiltInType>(leftType)) {
            if (llvm::isa<BuiltInType>(rightType)) {
                // For now, if both types are built in we use the built in operators
                // TODO: Once we support extension types we need to support calling the overridden operator functions
                //       for the built in types...
                // TODO: Implement the casting rules.
                CastSide castSide = CastHelper::getSideToCast(llvm::dyn_cast<BuiltInType>(leftType),
                                                              llvm::dyn_cast<BuiltInType>(rightType));

                switch (castSide) {
                    case CastSide::Left:
                        // Cast the left value to the right value's type
                        binaryOperatorExpr->leftValue = applyCast(leftType, rightType,
                                                                  binaryOperatorExpr->leftValue,
                                                                  CastOperatorType::Implicit);
                        break;
                    case CastSide::Right:
                        // Cast the right value to the left value's type
                        binaryOperatorExpr->rightValue = applyCast(rightType, leftType,
                                                                   binaryOperatorExpr->rightValue,
                                                                   CastOperatorType::Implicit);
                        break;
                    default:
                        // We don't do anything...
                        break;
                }

                // TODO: I believe we need to also call `dereferenceReferences` here.
                convertLValueToRValue(binaryOperatorExpr->leftValue);
                convertLValueToRValue(binaryOperatorExpr->rightValue);

                // We set the result type of the binary operator explicitly to the live value of the left value's
                // result type. We don't reference `leftType` here as it might not be the correct type.
                binaryOperatorExpr->resultType = binaryOperatorExpr->leftValue->resultType->deepCopy();

                // We don't do further processing
                return;
            } else {
                printError("built in type `" + leftType->getString() + "` currently only supports built in operators!",
                           binaryOperatorExpr->startPosition(), binaryOperatorExpr->endPosition());
            }
        } else if (llvm::isa<StructType>(leftType)) {
            auto structType = llvm::dyn_cast<StructType>(leftType);
            auto structDecl = structType->decl();

            std::vector<Expr*> operatorArgs {
                binaryOperatorExpr->rightValue
            };

            OperatorDecl* foundOperator = findInfixOperatorOrError(structDecl, &operatorArgs,
                                                                   binaryOperatorExpr->operatorName(),
                                                                   binaryOperatorExpr->startPosition(),
                                                                   binaryOperatorExpr->endPosition());

            // TODO: We need to check if `rightValue` needs referenced, dereferenced, or implicitly casted...

            auto resultType = foundOperator->resultType->deepCopy();

            // NOTE: Currently operator calls ALWAYS support vtable calls, we can later optimize these out where
            //       possible
            auto newExpr = new CustomInfixOperatorCallExpr(binaryOperatorExpr->startPosition(),
                                                           binaryOperatorExpr->endPosition(),
                                                           structType->doVTableCalls() && foundOperator->isVirtual(),
                                                           foundOperator,
                                                           binaryOperatorExpr->leftValue,
                                                           binaryOperatorExpr->rightValue);
            newExpr->resultType = resultType;
            // Like functions, operator results are lvalues
            newExpr->resultType->setIsLValue(true);

            // We steal these pointers
            binaryOperatorExpr->leftValue = nullptr;
            binaryOperatorExpr->rightValue = nullptr;
            // Delete the old binary operator and replace it with our new expression
            delete binaryOperatorExpr;
            expr = newExpr;

            // If the found operator is in a namespace we need to extern it (as we assume it could be in another
            // file or not declared yet)
            if (foundOperator->parentNamespace != nullptr) {
                currentFileAst->addImportExtern(foundOperator);
            }
        } else {
            printError("operator `" + binaryOperatorExpr->operatorName() + "` used on unsupported type '" +
                       binaryOperatorExpr->leftValue->resultType->getString() + "'!",
                       binaryOperatorExpr->startPosition(), binaryOperatorExpr->endPosition());
        }
    }
}
// TODO: Remove this
//void DeclResolver::processBinaryOperatorExpr(Expr*& expr, bool isNestedBinaryOperator) {
//    auto binaryOperatorExpr = llvm::dyn_cast<BinaryOperatorExpr>(expr);
//
//    // TODO: Support operator overloading type resolution
//    if (llvm::isa<BinaryOperatorExpr>(binaryOperatorExpr->leftValue)) {
//        processBinaryOperatorExpr(binaryOperatorExpr->leftValue, true);
//
//        // If the left value after processing isn't a local variable declaration then we continue like normal
//        // else we send the local variable decl into the `processLocalVariableDecl` since it wasn't performed in
//        // `processBinaryOperatorExpr` since we told it not to
//        if (llvm::isa<LocalVariableDeclExpr>(binaryOperatorExpr->leftValue)) {
//            processExpr(binaryOperatorExpr->rightValue);
//
//            processLocalVariableDeclExpr(llvm::dyn_cast<LocalVariableDeclExpr>(binaryOperatorExpr->leftValue), true);
//
//            binaryOperatorExpr->resultType = binaryOperatorExpr->leftValue->resultType->deepCopy();
//
//            // We don't do further processing
//            return;
//        }
//    } else {
//        processExpr(binaryOperatorExpr->leftValue);
//    }
//
//    if (llvm::isa<ResolvedTypeRefExpr>(binaryOperatorExpr->leftValue)) {
//        // TODO: Support the other type suffixes
//        if (binaryOperatorExpr->operatorName() == "*") {
//            if (!llvm::isa<IdentifierExpr>(binaryOperatorExpr->rightValue)) {
//                printError("expected local variable name after pointer type!",
//                           binaryOperatorExpr->rightValue->startPosition(),
//                           binaryOperatorExpr->rightValue->endPosition());
//                return;
//            }
//
//            auto localVarTypeRef = llvm::dyn_cast<ResolvedTypeRefExpr>(binaryOperatorExpr->leftValue);
//            auto varNameExpr = llvm::dyn_cast<IdentifierExpr>(binaryOperatorExpr->rightValue);
//
//            if (varNameExpr->hasTemplateArguments()) {
//                printError("local variable name cannot have template arguments!",
//                           varNameExpr->startPosition(),
//                           varNameExpr->endPosition());
//                return;
//            }
//
//            binaryOperatorExpr->leftValue = nullptr;
//
//            std::string varName = varNameExpr->name();
//
//            localVarTypeRef->resolvedType = new PointerType(localVarTypeRef->startPosition(),
//                                                            localVarTypeRef->endPosition(),
//                                                            TypeQualifier::None,
//                                                            localVarTypeRef->resolvedType);
//
//            auto localVarDeclExpr = new LocalVariableDeclExpr(binaryOperatorExpr->startPosition(),
//                                                              binaryOperatorExpr->endPosition(), localVarTypeRef,
//                                                              varName);
//
//            // TODO: Process the local variable in a nested
//            // We only process the local variable here if it is not a part of a nested binary operator
//            // This is so we can support handling initial value handling within the `processLocalVariableDeclExpr`
//            // function.
//            if (!isNestedBinaryOperator) {
//                processLocalVariableDeclExpr(localVarDeclExpr, false);
//            }
//
//            // Delete the binary operator expression (this will delete the left and right expressions)
//            delete binaryOperatorExpr;
//            expr = localVarDeclExpr;
//
//            return;
//        } else if (binaryOperatorExpr->operatorName() == "&") {
//            if (!llvm::isa<IdentifierExpr>(binaryOperatorExpr->rightValue)) {
//                printError("expected local variable name after reference type!",
//                           binaryOperatorExpr->rightValue->startPosition(),
//                           binaryOperatorExpr->rightValue->endPosition());
//                return;
//            }
//
//            auto localVarTypeRef = llvm::dyn_cast<ResolvedTypeRefExpr>(binaryOperatorExpr->leftValue);
//            auto varNameExpr = llvm::dyn_cast<IdentifierExpr>(binaryOperatorExpr->rightValue);
//
//            if (varNameExpr->hasTemplateArguments()) {
//                printError("local variable name cannot have template arguments!",
//                           varNameExpr->startPosition(),
//                           varNameExpr->endPosition());
//                return;
//            }
//
//            binaryOperatorExpr->leftValue = nullptr;
//
//            std::string varName = varNameExpr->name();
//
//            localVarTypeRef->resolvedType = new ReferenceType(localVarTypeRef->startPosition(),
//                                                            localVarTypeRef->endPosition(),
//                                                            TypeQualifier::None,
//                                                            localVarTypeRef->resolvedType);
//
//            auto localVarDeclExpr = new LocalVariableDeclExpr(binaryOperatorExpr->startPosition(),
//                                                              binaryOperatorExpr->endPosition(), localVarTypeRef,
//                                                              varName);
//
//            // TODO: Process the local variable in a nested
//            // We only process the local variable here if it is not a part of a nested binary operator
//            // This is so we can support handling initial value handling within the `processLocalVariableDeclExpr`
//            // function.
//            if (!isNestedBinaryOperator) {
//                processLocalVariableDeclExpr(localVarDeclExpr, false);
//            }
//
//            // Delete the binary operator expression (this will delete the left and right expressions)
//            delete binaryOperatorExpr;
//
//            expr = localVarDeclExpr;
//
//            return;
//        } else {
//            printError("unsupported binary operator expression with a type as an lvalue!",
//                       binaryOperatorExpr->startPosition(),
//                       binaryOperatorExpr->endPosition());
//            return;
//        }
//    }
//
//    processExpr(binaryOperatorExpr->rightValue);
//
//    if (binaryOperatorExpr->isBuiltInAssignmentOperator()) {
//        Type* leftType = binaryOperatorExpr->leftValue->resultType;
//        Type* rightType = binaryOperatorExpr->rightValue->resultType;
//
//        if (getTypeIsReference(leftType)) {
//            // If the left type is a reference and the left value is a local variable declaration then we set the left variable to the reference of what is on the right...
//            // If the left value ISN'T a local variable then we deref the left value and set it equal to the right value (with the right value being converted to an rvalue)
//            if (llvm::isa<LocalVariableDeclExpr>(binaryOperatorExpr->leftValue)) {
//                // Right value must be either lvalue or reference
//                if (!(rightType->isLValue() || getTypeIsReference(rightType))) {
//                    printError("initial value for reference variable MUST be an lvalue!",
//                               binaryOperatorExpr->rightValue->startPosition(),
//                               binaryOperatorExpr->rightValue->endPosition());
//                    return;
//                }
//
//                // If the right type isn't already a reference then make it one...
//                if (!getTypeIsReference(rightType)) {
//                    binaryOperatorExpr->rightValue = new PrefixOperatorExpr(
//                            binaryOperatorExpr->rightValue->startPosition(),
//                            binaryOperatorExpr->rightValue->endPosition(),
//                            ".ref", binaryOperatorExpr->rightValue);
//                    processExpr(binaryOperatorExpr->rightValue);
//                    rightType = binaryOperatorExpr->rightValue->resultType;
//                }
//            } else {
//                // This will dereference the left value
//                dereferenceReferences(binaryOperatorExpr->leftValue);
//                leftType = binaryOperatorExpr->leftValue->resultType;
//
//                // The right value here MUST be an rvalue...
//                convertLValueToRValue(binaryOperatorExpr->rightValue);
//                rightType = binaryOperatorExpr->rightValue->resultType;
//            }
//        } else {
//            // If the left value isn't a reference then the right value has to be an rvalue...
//            convertLValueToRValue(binaryOperatorExpr->rightValue);
//            rightType = binaryOperatorExpr->rightValue->resultType;
//        }
//
//        if (!TypeComparer::getTypesAreSame(leftType, rightType, true)) {
//            binaryOperatorExpr->rightValue = new ImplicitCastExpr(binaryOperatorExpr->rightValue->startPosition(),
//                                                                  binaryOperatorExpr->rightValue->endPosition(),
//                                                                  leftType->deepCopy(),
//                                                                  binaryOperatorExpr->rightValue);
//        }
//
//        binaryOperatorExpr->resultType = leftType->deepCopy();
//
//        if (binaryOperatorExpr->operatorName() != "=") {
//            if (llvm::isa<LocalVariableDeclExpr>(binaryOperatorExpr->leftValue)) {
//                printError("operator '" + binaryOperatorExpr->operatorName() + "' not supported in variable declaration!",
//                           binaryOperatorExpr->startPosition(), binaryOperatorExpr->endPosition());
//                return;
//            }
//
//            std::string rightOperatorName;
//
//            if (binaryOperatorExpr->operatorName() == ">>=") {
//                rightOperatorName = ">>";
//            } else if (binaryOperatorExpr->operatorName() == "<<=") {
//                rightOperatorName = "<<";
//            } else if (binaryOperatorExpr->operatorName() == "+=") {
//                rightOperatorName = "+";
//            } else if (binaryOperatorExpr->operatorName() == "-=") {
//                rightOperatorName = "-";
//            } else if (binaryOperatorExpr->operatorName() == "*=") {
//                rightOperatorName = "*";
//            } else if (binaryOperatorExpr->operatorName() == "/=") {
//                rightOperatorName = "/";
//            } else if (binaryOperatorExpr->operatorName() == "%=") {
//                rightOperatorName = "%";
//            } else if (binaryOperatorExpr->operatorName() == "&=") {
//                rightOperatorName = "&";
//            } else if (binaryOperatorExpr->operatorName() == "|=") {
//                rightOperatorName = "|";
//            } else if (binaryOperatorExpr->operatorName() == "^=") {
//                rightOperatorName = "^";
//            } else {
//                printError(
//                        "[INTERNAL] unknown built in assignment operator '" + binaryOperatorExpr->operatorName() + "'!",
//                        binaryOperatorExpr->startPosition(), binaryOperatorExpr->endPosition());
//                return;
//            }
//
//            Expr* leftValue = binaryOperatorExpr->leftValue;
//            convertLValueToRValue(leftValue);
//            // We set a new right value with an `LValueToRValueExpr` that doesn't own the pointer. Making it so the left L2R expression doesn't delete the pointer since the binary operator owns it.
//            auto newRightValue = new BinaryOperatorExpr(binaryOperatorExpr->startPosition(),
//                                                        binaryOperatorExpr->endPosition(),
//                                                        rightOperatorName,
//                                                        leftValue,
//                                                        binaryOperatorExpr->rightValue);
//
//            newRightValue->resultType = leftType->deepCopy();
//
//            // Set the new right value and change the operator name to '='
//            binaryOperatorExpr->setOperatorName("=");
//            binaryOperatorExpr->rightValue = newRightValue;
//        } else {
//            // At this point the operator is an "="
//            if (currentFunctionIsConstructor &&
//                    llvm::isa<RefStructMemberVariableExpr>(binaryOperatorExpr->leftValue)) {
//                // If the left value is a struct member variable then we have to set `constructorAssignedMember` when
//                // we're in a constructor
//                auto refStructMemberVariable = llvm::dyn_cast<RefStructMemberVariableExpr>(binaryOperatorExpr->leftValue);
//
//                if (llvm::isa<StructType>(refStructMemberVariable->refVariable->type)) {
//                    // We only do this when it is a struct type...
//                    if (refStructMemberVariable->structType->decl() == currentStruct) {
//                        // We only check if the struct is the same type as our current type (we also don't set
//                        // `constructorAssignedMember` if it is a base member we're setting)
//
//                        if (std::find(constructorAssignedMember.begin(), constructorAssignedMember.end(),
//                                      refStructMemberVariable->refVariable) == constructorAssignedMember.end()) {
//                            // If the variable being set isn't in `constructorAssignedMember` then we have to add it
//                            constructorAssignedMember.push_back(refStructMemberVariable->refVariable);
//                            // TODO: We need to convert a move constructor here WITHOUT destructing the member variable
//                            //       (because the member variable is garbage right now, this will be the first construction)
//                            //       We might want a special `assign` constructor for only this scenario? Or how should we
//                            //       perform this?
//                        }
//                    }
//                }
//            }
//        }
//
//        return;
//    } else {
//        convertLValueToRValue(binaryOperatorExpr->leftValue);
//        convertLValueToRValue(binaryOperatorExpr->rightValue);
//
//        Type* leftType = binaryOperatorExpr->leftValue->resultType;
//        Type* rightType = binaryOperatorExpr->rightValue->resultType;
//
//        if (llvm::isa<BuiltInType>(leftType) && llvm::isa<BuiltInType>(rightType)) {
//            auto leftBuiltInType = llvm::dyn_cast<BuiltInType>(leftType);
//            auto rightBuiltInType = llvm::dyn_cast<BuiltInType>(rightType);
//
//            bool leftIsSigned = leftBuiltInType->isSigned();
//            bool rightIsSigned = rightBuiltInType->isSigned();
//            bool leftIsFloating = leftBuiltInType->isFloating();
//            bool rightIsFloating = rightBuiltInType->isFloating();
//
//            // If one side is signed and the other isn't then we cast to the signed side.
//            if (leftIsSigned) {
//                if (!rightIsSigned) {
//                    binaryOperatorExpr->leftValue = new ImplicitCastExpr(binaryOperatorExpr->leftValue->startPosition(),
//                                                                         binaryOperatorExpr->leftValue->endPosition(),
//                                                                         rightType->deepCopy(),
//                                                                         binaryOperatorExpr->leftValue);
//                    binaryOperatorExpr->resultType = rightType->deepCopy();
//                    return;
//                }
//            } else { // left is not signed...
//                if (rightIsSigned) {
//                    binaryOperatorExpr->rightValue = new ImplicitCastExpr(binaryOperatorExpr->rightValue->startPosition(),
//                                                                          binaryOperatorExpr->rightValue->endPosition(),
//                                                                          leftType->deepCopy(),
//                                                                          binaryOperatorExpr->rightValue);
//                    binaryOperatorExpr->resultType = leftType->deepCopy();
//                    return;
//                }
//            }
//
//            // If one side is floating and the other isn't then we cast to the floating side.
//            if (leftIsFloating) {
//                if (!rightIsFloating) {
//                    binaryOperatorExpr->leftValue = new ImplicitCastExpr(binaryOperatorExpr->leftValue->startPosition(),
//                                                                         binaryOperatorExpr->leftValue->endPosition(),
//                                                                         rightType->deepCopy(),
//                                                                         binaryOperatorExpr->leftValue);
//                    binaryOperatorExpr->resultType = rightType->deepCopy();
//                    return;
//                }
//            } else { // left is not signed...
//                if (rightIsFloating) {
//                    binaryOperatorExpr->rightValue = new ImplicitCastExpr(binaryOperatorExpr->rightValue->startPosition(),
//                                                                          binaryOperatorExpr->rightValue->endPosition(),
//                                                                          leftType->deepCopy(),
//                                                                          binaryOperatorExpr->rightValue);
//                    binaryOperatorExpr->resultType = leftType->deepCopy();
//                    return;
//                }
//            }
//
//            // If one side is bigger than the other then we casting to the bigger side.
//            if (leftBuiltInType->size() > rightBuiltInType->size()) {
//                binaryOperatorExpr->rightValue = new ImplicitCastExpr(binaryOperatorExpr->rightValue->startPosition(),
//                                                                      binaryOperatorExpr->rightValue->endPosition(),
//                                                                      leftType->deepCopy(),
//                                                                      binaryOperatorExpr->rightValue);
//                binaryOperatorExpr->resultType = leftType->deepCopy();
//                return;
//            } else if (leftBuiltInType->size() < rightBuiltInType->size()) {
//                binaryOperatorExpr->leftValue = new ImplicitCastExpr(binaryOperatorExpr->leftValue->startPosition(),
//                                                                     binaryOperatorExpr->leftValue->endPosition(),
//                                                                     rightType->deepCopy(),
//                                                                     binaryOperatorExpr->leftValue);
//                binaryOperatorExpr->resultType = rightType->deepCopy();
//                return;
//            }
//
//            // If we reach this point then both are exactly the same type in everything but name. We do nothing if there isn't a custom implicit cast operator for each type name
//        }
//    }
//
//    binaryOperatorExpr->resultType = binaryOperatorExpr->leftValue->resultType->deepCopy();
//}

void DeclResolver::processCharacterLiteralExpr(CharacterLiteralExpr *characterLiteralExpr) {
    // TODO: Type suffix support
    characterLiteralExpr->resultType = new BuiltInType({}, {}, TypeQualifier::None, "char");
}

void DeclResolver::processExplicitCastExpr(Expr*& expr) {
    auto explicitCastExpr = llvm::dyn_cast<ExplicitCastExpr>(expr);

    // Convert any template type references to the supplied type
    applyTemplateTypeArguments(explicitCastExpr->castType);

    processExpr(explicitCastExpr->castee);

    Type* castType = explicitCastExpr->castType;
    Expr* castee = explicitCastExpr->castee;

    // We steal these pointers
    explicitCastExpr->castType = nullptr;
    explicitCastExpr->castee = nullptr;
    delete explicitCastExpr;

    expr = applyCast(castee->resultType, castType, castee, CastOperatorType::Explicit);

    // This is copied in `applyCast`, we delete it since it is no longer in use
    delete castType;
}

void DeclResolver::processFloatLiteralExpr(FloatLiteralExpr *floatLiteralExpr) {
    // TODO: Type suffix support
    floatLiteralExpr->resultType = new BuiltInType({}, {}, TypeQualifier::None, "float");
}

void DeclResolver::processFunctionCallExpr(Expr*& expr) {
    auto functionCallExpr = llvm::dyn_cast<FunctionCallExpr>(expr);

    if (functionCallExpr->hasArguments()) {
        // TODO: Support overloading the operator ()
        for (Expr*& arg : functionCallExpr->arguments) {
            processExpr(arg);

            // NOTE: `convertLValueToRValue` is called AFTER resolving the declaration...
            //convertLValueToRValue(arg);
        }

        functionCallArgs = &functionCallExpr->arguments;
    }

    // If `functionCallExpr->functionReference` is a type reference then the function call is actually a constructor call
    // TODO: Should we move some of this to `TypeResolver`?
    if (llvm::isa<ResolvedTypeRefExpr>(functionCallExpr->functionReference)) {
        auto resolvedTypeRef = llvm::dyn_cast<ResolvedTypeRefExpr>(functionCallExpr->functionReference);

        // TODO: We need to allow constructing ANY type (except traits, abstracts, etc.) with a single argument
        //       e.g. `int i = int(12);`, `EnumType et = EnumType(enumVar)`
        if (!llvm::isa<StructType>(resolvedTypeRef->resolvedType)) {
            printError("type `" + resolvedTypeRef->resolvedType->getString() +
                       "` does not have any constructors!",
                       functionCallExpr->functionReference->startPosition(),
                       functionCallExpr->functionReference->endPosition());
        }

        auto structType = llvm::dyn_cast<StructType>(resolvedTypeRef->resolvedType);
        auto structDecl = structType->decl();

        ConstructorDecl* foundConstructor = nullptr;
        // `isExactMatch` is used to check for ambiguity
        bool isExactMatch = false;
        bool isAmbiguous = false;

        if (!functionCallExpr->hasArguments()) {
            // If there aren't any arguments we grab the public, default constructor
            for (ConstructorDecl* constructorDecl : structDecl->constructors) {
                // Skip any constructors not visible to us
                if (!VisibilityChecker::canAccessStructMember(structType, currentStruct,
                                                              constructorDecl)) {
                    continue;
                }

                // TODO: We should account for constructors that have parameters but they all have defaults...
                if (!constructorDecl->hasParameters()) {
                    foundConstructor = constructorDecl;
                    isExactMatch = true;
                    break;
                }
            }
        } else {
            for (ConstructorDecl *constructorDecl : structDecl->constructors) {
                // Skip any constructors not visible to us
                if (!VisibilityChecker::canAccessStructMember(structType, currentStruct,
                                                              constructorDecl)) {
                    continue;
                }

                if (!checkConstructorOrFunctionMatchesCall(foundConstructor, constructorDecl,
                                                           &functionCallExpr->arguments,
                                                           &isExactMatch, &isAmbiguous)) {
                    printError("constructor call is ambiguous!",
                               functionCallExpr->startPosition(), functionCallExpr->endPosition());
                }
            }
        }

        if (foundConstructor) {
            if (isAmbiguous) {
                printError("constructor call is ambiguous!",
                           functionCallExpr->startPosition(), functionCallExpr->endPosition());
            }

            if (!isExactMatch) {
                // TODO: Implicitly cast and extract default values from the constructor parameter list
            }

            currentFileAst->addImportExtern(foundConstructor);

            // Since we found a found a constructor we have to convert any lvalues to rvalues and
            // dereference references where it makes sense...
            for (std::size_t i = 0; i < foundConstructor->parameters.size(); ++i) {
                if (!getTypeIsReference(foundConstructor->parameters[i]->type)) {
                    dereferenceReferences(functionCallExpr->arguments[i]);
                    convertLValueToRValue(functionCallExpr->arguments[i]);
                }
            }

            // Replace the function call with the constructor call
            auto newResult = new ConstructTemporaryValueExpr(expr->startPosition(), expr->endPosition(),
                                                             foundConstructor, std::move(functionCallExpr->arguments));
            newResult->resultType = structType->deepCopy();
            newResult->resultType->setIsLValue(true);
            delete functionCallExpr;
            expr = newResult;
        } else {
            printError("`" + structDecl->name() + "` "
                       "does not have a constructor that matches the provided parameters!",
                       functionCallExpr->startPosition(), functionCallExpr->endPosition());
        }
    } else {
        // If the `functionCallExpr` isn't a type then we try to resolve what it is
        exprIsFunctionCall = true;
        // We process the function reference expression to HOPEFULLY have `functionCallExpr->resultType` be a `FunctionPointerType` if it isn't then we error
        processExpr(functionCallExpr->functionReference);
        exprIsFunctionCall = false;

        if (llvm::isa<TempNamespaceRefExpr>(functionCallExpr->functionReference) ||
            functionCallExpr->functionReference->resultType == nullptr) {
            // This will trigger an error in `CodeVerifier`, this most likely means the function that was called is named the same as a namespace
            functionCallExpr->resultType = new BuiltInType({}, {}, TypeQualifier::None, "int32");
            functionCallArgs = nullptr;
            return;
        }

        auto checkType = functionCallExpr->functionReference->resultType;

        // TODO: Support `ConstType`, `MutType`, and `ImmutType` since they can all contain `FunctionPointerType`
        if (llvm::isa<FunctionPointerType>(checkType)) {
            auto functionPointerType = llvm::dyn_cast<FunctionPointerType>(
                    functionCallExpr->functionReference->resultType);

            for (std::size_t i = 0; i < functionCallExpr->arguments.size(); ++i) {
                Type *checkParamType = functionPointerType->paramTypes[i];
                Type *checkArgType = functionCallExpr->arguments[i]->resultType;

                if (llvm::isa<ReferenceType>(checkParamType))
                    checkParamType = llvm::dyn_cast<ReferenceType>(checkParamType)->referenceToType;
                if (llvm::isa<ReferenceType>(checkArgType))
                    checkArgType = llvm::dyn_cast<ReferenceType>(checkArgType)->referenceToType;

                if (!TypeComparer::getTypesAreSame(checkParamType, checkArgType)) {
                    // We will handle this in the verifier.
                    functionCallExpr->arguments[i] = applyCast(functionCallExpr->arguments[i]->resultType,
                                                               functionPointerType->paramTypes[i],
                                                               functionCallExpr->arguments[i],
                                                               CastOperatorType::Implicit);
                }

                if (!getTypeIsReference(functionPointerType->paramTypes[i])) {
                    convertLValueToRValue(functionCallExpr->arguments[i]);
                }
            }

            // We set the result type of this expression to the result type of the function being called.
            functionCallExpr->resultType = functionPointerType->resultType->deepCopy();
            // Function results are put onto the stack to make them referencable. Because of this we have to mark them as
            // an `lvalue`
            // TODO: We might want to make a `readonly` type qualifier for function results.
            //       They ARE NOT settable but aren't strict-const, they're implicitly mutable.
            functionCallExpr->resultType->setIsLValue(true);
        } else if (llvm::isa<StructType>(checkType)) {
            auto structType = llvm::dyn_cast<StructType>(checkType);
            auto structDecl = structType->decl();

            CallOperatorDecl* foundCallOperator = nullptr;
            // `isExactMatch` is used to check for ambiguity
            bool isExactMatch = false;
            bool isAmbiguous = false;

            for (Decl* decl : structDecl->members) {
                // Skip any decls not visible to us
                if (!VisibilityChecker::canAccessStructMember(structType, currentStruct,
                                                              decl)) {
                    continue;
                }

                if (llvm::isa<CallOperatorDecl>(decl)) {
                    auto checkCallOperatorDecl = llvm::dyn_cast<CallOperatorDecl>(decl);

                    if (!checkConstructorOrFunctionMatchesCall(foundCallOperator, checkCallOperatorDecl,
                                                               &functionCallExpr->arguments,
                                                               &isExactMatch, &isAmbiguous)) {
                        printError("struct call operator use is ambiguous!",
                                   functionCallExpr->startPosition(), functionCallExpr->endPosition());
                    }
                }
            }

            if (foundCallOperator == nullptr) {
                for (Decl* decl : structDecl->inheritedMembers) {
                    // Skip any decls not visible to us
                    if (!VisibilityChecker::canAccessStructMember(structType, currentStruct,
                                                                  decl)) {
                        continue;
                    }

                    if (llvm::isa<CallOperatorDecl>(decl)) {
                        auto checkCallOperatorDecl = llvm::dyn_cast<CallOperatorDecl>(decl);

                        if (!checkConstructorOrFunctionMatchesCall(foundCallOperator, checkCallOperatorDecl,
                                                                   &functionCallExpr->arguments,
                                                                   &isExactMatch, &isAmbiguous)) {
                            printError("struct call operator use is ambiguous!",
                                       functionCallExpr->startPosition(), functionCallExpr->endPosition());
                        }
                    }
                }
            }

            if (foundCallOperator != nullptr) {
                if (isAmbiguous) {
                    printError("struct call operator use is ambiguous!",
                               functionCallExpr->startPosition(), functionCallExpr->endPosition());
                }

                if (!isExactMatch) {
                    // TODO: Implicitly cast and extract default values from the constructor parameter list
                }

                currentFileAst->addImportExtern(foundCallOperator);

                auto newExpr = new CustomCallOperatorCallExpr(expr->startPosition(), expr->endPosition(),
                                                              structType->doVTableCalls() && foundCallOperator->isVirtual(),
                                                              foundCallOperator,
                                                              functionCallExpr->functionReference,
                                                              std::move(functionCallExpr->arguments));

                newExpr->resultType = foundCallOperator->resultType->deepCopy();
                newExpr->resultType->setIsLValue(true);

                // We steal this pointer
                functionCallExpr->functionReference = nullptr;

                // Replace the function call with our custom call operator call
                delete functionCallExpr;
                expr = newExpr;

                for (std::size_t i = 0; i < foundCallOperator->parameters.size(); ++i) {
                    if (!getTypeIsReference(foundCallOperator->parameters[i]->type)) {
                        dereferenceReferences(newExpr->arguments[i]);
                        convertLValueToRValue(newExpr->arguments[i]);
                    }
                }
            } else {
                printError("struct `" + structDecl->name() + "` does not have a call operator with the provided parameter types!",
                           functionCallExpr->startPosition(), functionCallExpr->endPosition());
            }
        } else {
            printError("expression is not a valid function reference!",
                       functionCallExpr->functionReference->startPosition(),
                       functionCallExpr->functionReference->endPosition());
        }
    }

    functionCallArgs = nullptr;
}

bool DeclResolver::canImplicitCast(const Type* to, const Type* from) {
    // You CANNOT implicitly cast from const to non-const.
    if (from->qualifier() == TypeQualifier::Const && (to->qualifier() != TypeQualifier::Const)) {
        return false;
    }

    if (llvm::isa<BuiltInType>(to)) {
        if (llvm::isa<BuiltInType>(from)) {
            // If `to` and `from` are both built in types then they CAN be implicitly casted...
            return true;
        }
    } else if (llvm::isa<PointerType>(to)) {
        auto toPtr = llvm::dyn_cast<PointerType>(to);

        if (llvm::isa<PointerType>(from)) {
            auto fromPtr = llvm::dyn_cast<PointerType>(from);
            return canImplicitCast(toPtr->pointToType, fromPtr->pointToType);
        }
    } else if (llvm::isa<StructType>(to)) {
        auto toStruct = llvm::dyn_cast<StructType>(to);

        if (llvm::isa<StructType>(from)) {
            auto fromStruct = llvm::dyn_cast<StructType>(from);

            // TODO: We need to check for overloaded casts
            if (toStruct->decl() == fromStruct->decl()) {
                // Obviously if the structs are the same they can be `implicitly casted` which is a no-op for the same
                // type
                return true;
            }
        }
    }

    return false;
}

bool DeclResolver::argsMatchParams(const std::vector<ParameterDecl*> &params, const std::vector<Expr*>* args,
                                   bool *isExact,
                                   const std::vector<TemplateParameterDecl*>& functionCallTemplateParams,
                                   const std::vector<Expr*>& functionCallTemplateArgs) {
    // If there are more args than there are params then the args can never match
    // NOTE: There can be more `params` than `args` but all params after `args` MUST be optional.
    if (args != nullptr && args->size() > params.size()) return false;

    if (params.empty() && (args == nullptr || args->empty())) {
        *isExact = true;
        return true;
    }

    bool currentlyExactMatch = true;

    for (std::size_t i = 0; i < params.size(); ++i) {
        // If `i` is greater than the size of `args` then the parameter at that index MUST be optional (a.k.a. has a default argument)
        if (args == nullptr || i >= args->size()) {
            *isExact = currentlyExactMatch;
            return params[i]->hasDefaultArgument();
        }

        Type* argType = (*args)[i]->resultType;
        Type* paramType = params[i]->type;

        if (llvm::isa<FunctionTemplateTypenameRefType>(paramType)) {
            auto funcTemplateTypenameRef = llvm::dyn_cast<FunctionTemplateTypenameRefType>(paramType);

            std::size_t paramIndex = funcTemplateTypenameRef->templateParameterIndex();
            const Expr* typeRef = nullptr;

            if (funcTemplateTypenameRef->templateParameterIndex() >= functionCallTemplateArgs.size()) {
                if (!functionCallTemplateParams[paramIndex]->hasDefaultArgument()) {
                    printDebugWarning("template function call has missing default argument type!");
                    continue;
                }

                typeRef = functionCallTemplateParams[paramIndex]->defaultArgument();
            } else {
                typeRef = functionCallTemplateArgs[paramIndex];
            }

            if (!llvm::isa<ResolvedTypeRefExpr>(typeRef)) {
                printDebugWarning("template function call has unresolved type ref!");
                continue;
            }

            auto resolvedTypeRef = llvm::dyn_cast<ResolvedTypeRefExpr>(typeRef);

            paramType = resolvedTypeRef->resolvedType;
        }

        // We ignore reference types since references are passed to functions the same as non-references in how they're called
        // We DO NOT ignore the qualifiers on what the reference type is referencing, `int const&` != `int mut&`
        if (llvm::isa<ReferenceType>(argType)) argType = llvm::dyn_cast<ReferenceType>(argType)->referenceToType;
        if (llvm::isa<ReferenceType>(paramType)) paramType = llvm::dyn_cast<ReferenceType>(paramType)->referenceToType;

        if (!TypeComparer::getTypesAreSame(argType, paramType, false)) {
            if (canImplicitCast(paramType, argType)) {
                currentlyExactMatch = false;
                continue;
            } else {
                return false;
            }
        }
    }

    *isExact = currentlyExactMatch;
    return true;
}

bool DeclResolver::templateArgsMatchParams(const std::vector<TemplateParameterDecl*>& params,
                                           const std::vector<Expr*>& args) {
    for (std::size_t i = 0; i < params.size(); ++i) {
        if (i >= args.size()) {
            // If the index is greater than the number of arguments we have then there is only a match if the parameter at the index is optional
            return params[i]->hasDefaultArgument();
        } else {
            if (llvm::isa<TemplateTypenameType>(params[i]->type)) {
                // If the parameter is a template type (i.e. `<typename T>` then the argument MUST be a resolved type reference
                if (!llvm::isa<ResolvedTypeRefExpr>(args[i])) {
                    if (llvm::isa<UnresolvedTypeRefExpr>(args[i])) {
                        printError("[INTERNAL] unresolved type found in template parameter checker!",
                                   args[i]->startPosition(), args[i]->endPosition());
                    }

                    return false;
                }
            } else {
                if (!llvm::isa<BuiltInType>(params[i]->type)) {
                    printError("[INTERNAL] only built in types supported for template parameters!",
                               args[i]->startPosition(), args[i]->endPosition());
                }

                auto builtInType = llvm::dyn_cast<BuiltInType>(params[i]->type);

                if (builtInType->isFloating()) {
                    if (!(llvm::isa<FloatLiteralExpr>(args[i]) || llvm::isa<IntegerLiteralExpr>(args[i]))) {
                        return false;
                    }
                } else {
                    if (!llvm::isa<IntegerLiteralExpr>(args[i])) {
                        return false;
                    }
                }
            }
        }
    }

    return true;
}

template<class T>
bool DeclResolver::checkConstructorOrFunctionMatchesCall(T*& currentFoundDecl, T* checkDecl,
                                                         const std::vector<Expr*>* args,
                                                         bool* isExactMatch, bool* isAmbiguous) {
    bool checkIsExactMatch = false;

    // We check if the args are a match with the function parameters
    if (argsMatchParams(checkDecl->parameters, args, &checkIsExactMatch)) {
        // If the current found function is an exact match and the checked function is exact then there is an ambiguity...
        if (*isExactMatch && checkIsExactMatch) {
            // We have to immediately exit if two functions are exact matches...
            return false;
            // If neither found function is an exact match then there is an ambiguity...
            //  BUT this kind of ambiguity can be solved if we find an exact match...
        } else if (!(*isExactMatch || checkIsExactMatch)) {
            if (currentFoundDecl == nullptr) {
                currentFoundDecl = checkDecl;
            } else {
                *isAmbiguous = true;
            }
        } else if (!*isExactMatch) {
            // If the new function is an exact match then `isAmbiguous` is now false
            if (checkIsExactMatch) {
                *isAmbiguous = false;
            }
            currentFoundDecl = checkDecl;
            *isExactMatch = checkIsExactMatch;
        }
    }

    return true;
}

bool DeclResolver::checkFunctionMatchesCall(FunctionDecl*& currentFoundFunction, FunctionDecl* checkFunction,
                                            bool* isExactMatch, bool* isAmbiguous) {
    return checkConstructorOrFunctionMatchesCall(currentFoundFunction, checkFunction,
                                                 functionCallArgs, isExactMatch, isAmbiguous);
}

bool DeclResolver::checkTemplateFunctionMatchesCall(FunctionDecl *&currentFoundFunction,
                                                    TemplateFunctionDecl *checkFunction,
                                                    bool *isExactMatch, bool *isAmbiguous,
                                                    std::vector<Expr*>& functionCallTemplateArgs) {
    if (functionCallTemplateArgs.size() > checkFunction->templateParameters.size()) {
        // Skip if there are more arguments than parameters...
        return false;
    }

    if (!templateArgsMatchParams(checkFunction->templateParameters, functionCallTemplateArgs)) {
        // Template arguments must match the parameters
        // TODO: We should probably do an `isExact` kinda thing. Literals are strongly typed.
        //  so if you do `example<1>` and there is an `example<i16 x>` and `example<i64 x>` this would be ambiguous
        //  but if there was an `example<i32 x>` then it would no longer be ambiguous
        return false;
    }

    bool checkIsExactMatch = false;
    bool createdNewFunction = false;

    FunctionDecl* implementedFunction = nullptr;

    // TODO: Can we convert this to use `checkConstructorOrFunctionMatchesCall`?
    // We check if the args are a match with the function parameters
    if (argsMatchParams(checkFunction->parameters, functionCallArgs, &checkIsExactMatch,
                        checkFunction->templateParameters, functionCallTemplateArgs)) {
        // If the current found function is an exact match and the checked function is exact then there is an ambiguity...
        if (*isExactMatch && checkIsExactMatch) {
            // We have to immediately exit if two functions are exact matches...
            return false;
            // If neither found function is an exact match then there is an ambiguity...
            //  BUT this kind of ambiguity can be solved if we find an exact match...
        } else if (!(*isExactMatch || checkIsExactMatch)) {
            if (currentFoundFunction == nullptr) {
                implementedFunction = checkFunction->getOrCreateFunction(functionCallTemplateArgs, &createdNewFunction);
                currentFoundFunction = implementedFunction;
            } else {
                *isAmbiguous = true;
            }
        } else if (!*isExactMatch) {
            // If the new function is an exact match then `isAmbiguous` is now false
            if (checkIsExactMatch) {
                *isAmbiguous = false;
            }
            implementedFunction = checkFunction->getOrCreateFunction(functionCallTemplateArgs, &createdNewFunction);
            currentFoundFunction = implementedFunction;
            *isExactMatch = checkIsExactMatch;
        }
    }

    if (createdNewFunction) {
        processTemplateFunctionDeclImplementation(checkFunction, functionCallTemplateArgs, implementedFunction);
    }

    return true;
}

bool DeclResolver::processIdentifierExprForDecl(Decl *checkDecl, IdentifierExpr* identifierExpr, bool hasTemplateArgs,
                                                FunctionDecl*& foundFunction, bool& isExactMatch, bool& isAmbiguous) {
    if (llvm::isa<GlobalVariableDecl>(checkDecl)) {
        // If there are template args then we have to find a template decl
        if (hasTemplateArgs) return false;

        // TODO: Instead of doing what we do below, we need to know if the current identifier is being used for a
        //       function call or now. If it isn't, then we should only look for global variables
        //       If it is used in a function call we should ignore any global variables that aren't function pointers
        // TODO: Reimplement this
//        if (foundGlobalVariable != nullptr || foundFunction != nullptr) {
//            printError("identifier '" + identifierExpr->name() + "' is ambiguous!",
//                       identifierExpr->startPosition(), identifierExpr->endPosition());
//            return;
//        }
        return true;
    } else if (llvm::isa<FunctionDecl>(checkDecl)) {
        // If there are template args then we have to find a template decl
        if (hasTemplateArgs) return false;

        auto functionDecl = llvm::dyn_cast<FunctionDecl>(checkDecl);

//        if (foundGlobalVariable != nullptr) {
//            printError("identifier '" + identifierExpr->name() + "' is ambiguous!",
//                       identifierExpr->startPosition(), identifierExpr->endPosition());
//            return;
//        }

        if (!checkFunctionMatchesCall(foundFunction, functionDecl, &isExactMatch, &isAmbiguous)) {
            printError("function call is ambiguous!",
                       identifierExpr->startPosition(), identifierExpr->endPosition());
        }
    } else if (llvm::isa<TemplateFunctionDecl>(checkDecl)) {
        // TODO: We should support implicit template typing (i.e. `int test<T>(T p);` calling `test(12);` == `test<int>(12);`
        if (!hasTemplateArgs) return false;

        auto templateFunctionDecl = llvm::dyn_cast<TemplateFunctionDecl>(checkDecl);

//        if (foundGlobalVariable != nullptr) {
//            printError("identifier '" + identifierExpr->name() + "' is ambiguous!",
//                       identifierExpr->startPosition(), identifierExpr->endPosition());
//            return;
//        }

        if (!checkTemplateFunctionMatchesCall(foundFunction, templateFunctionDecl, &isExactMatch, &isAmbiguous, identifierExpr->templateArguments)) {
            printError("function call is ambiguous!",
                       identifierExpr->startPosition(), identifierExpr->endPosition());
        }
    }

    return false;
}

/**
 * Try to find the `IdentifierExpr` within the current context by searching local variables, params, decls, etc.
 */
void DeclResolver::processIdentifierExpr(Expr*& expr) {
    auto identifierExpr = llvm::dyn_cast<IdentifierExpr>(expr);

    // Process the template arguments
    for (Expr*& templateArg : identifierExpr->templateArguments) {
        processExpr(templateArg);
    }

    // First we check if the identifier is a built in type
    if (BuiltInType::isBuiltInType(identifierExpr->name())) {
        // TODO: Support template overloading. Allow someone to implement `struct int<T> {}` that will be found if there are template arguments
        if (identifierExpr->hasTemplateArguments()) {
            printError("built in types do not support templating!",
                       identifierExpr->startPosition(), identifierExpr->endPosition());
        }

        Type *resolvedType = new BuiltInType(expr->startPosition(), expr->endPosition(), TypeQualifier::None,
                                             identifierExpr->name());

        delete identifierExpr;

        expr = new ResolvedTypeRefExpr(expr->startPosition(), expr->endPosition(), resolvedType);
        expr->resultType = resolvedType->deepCopy();
        return;
    }

    // This is a special identifier (but I don't want to go through the trouble of making it a real keyword right now,
    // it really doesn't need to be...)
    if (identifierExpr->name() == "this" || identifierExpr->name() == "base") {
        bool isBaseCall = identifierExpr->name() == "base";

        if (!currentStruct) {
            if (isBaseCall) {
                printError("use of keyword `base` outside of struct/class members is illegal!",
                           identifierExpr->startPosition(), identifierExpr->endPosition());
            } else {
                printError("use of keyword `this` outside of struct/class members is illegal!",
                           identifierExpr->startPosition(), identifierExpr->endPosition());
            }
        }

        if (identifierExpr->hasTemplateArguments()) {
            if (isBaseCall) {
                printError("keyword `base` does not support template arguments!",
                           identifierExpr->startPosition(), identifierExpr->endPosition());
            } else {
                printError("keyword `this` does not support template arguments!",
                           identifierExpr->startPosition(), identifierExpr->endPosition());
            }
        }

        // `this` is ALWAYS defined as parameter index `0` (when a function is a member of a struct/class, obviously
        // `0` isn't `this` to namespace or file functions)
        Expr* refParam = new RefParameterExpr(identifierExpr->startPosition(),
                                              identifierExpr->endPosition(),
                                              0);
        refParam->resultType = new ReferenceType({}, {}, TypeQualifier::None,
                                                 new StructType({}, {},
                                                                TypeQualifier::None,
                                                                currentStruct->name(),
                                                                currentStruct));
        // A parameter reference is an lvalue.
        refParam->resultType->setIsLValue(true);
        // NOTE: We still make it a `ReferenceType` above then immediately dereference it here so we create the correct
        //  expressions for dereferencing the `this` parameter in our `CodeGen`
        dereferenceReferences(refParam);

        // `this` and `base` are not assignable so we make them rvalues
        refParam->resultType->setIsLValue(false);

        delete identifierExpr;

        if (isBaseCall) {
            if (currentStruct->baseStruct == nullptr) {
                printError("struct '" + currentStruct->name() + "' does not implement a base type, cannot use `base` keyword!",
                           refParam->startPosition(), refParam->endPosition());
            }

            expr = new RefBaseExpr(refParam->startPosition(), refParam->endPosition(), refParam);
            auto baseType = new StructType({}, {}, TypeQualifier::None,
                                           currentStruct->baseStruct->name(),
                                           currentStruct->baseStruct);
            // Disallow using the vtable for any function calls
            baseType->setDoVTableCalls(false);
            expr->resultType = baseType;
        } else {
            // TODO: We should implement a `RefThisExpr` to make error checking easier
            expr = refParam;
        }
        return;
    }

    // Then we check local variables
    for (std::size_t i = 0; i < functionLocalVariablesCount; ++i) {
        if (functionLocalVariables[i]->name() == identifierExpr->name()) {
            if (identifierExpr->hasTemplateArguments()) {
                printError("local variable references cannot have template arguments!",
                           identifierExpr->startPosition(), identifierExpr->endPosition());
            }

            Expr* refLocal = new RefLocalVariableExpr(identifierExpr->startPosition(), identifierExpr->endPosition(),
                                                      identifierExpr->name());
            refLocal->resultType = functionLocalVariables[i]->resultType->deepCopy();
            // A local variable reference is an lvalue.
            refLocal->resultType->setIsLValue(true);
            // We always dereference local variable calls if they are references...
            dereferenceReferences(refLocal);

            delete identifierExpr;

            expr = refLocal;

            return;
        }
    }

    // Then we check template parameters
    if (functionTemplateParams != nullptr) {
        for (std::size_t i = 0; i < functionTemplateParams->size(); ++i) {
            if ((*functionTemplateParams)[i]->name() == identifierExpr->name()) {
                Expr* resultExpr = nullptr;

                if (functionTemplateArgs == nullptr || i >= functionTemplateArgs->size()) {
                    if (!(*functionTemplateParams)[i]->hasDefaultArgument()) {
                        // If the template arguments are null and the function template parameter doesn't have a
                        //  default value then the identifier isn't a template parameter... stop checking them.
                        break;
                    }

                    resultExpr = (*functionTemplateParams)[i]->defaultArgument()->deepCopy();
                } else {
                    resultExpr = (*functionTemplateArgs)[i]->deepCopy();
                }

                // Delete the old identifier and set expression where it used to reside to now be the template argument value.
                delete identifierExpr;
                expr = resultExpr;

                return;
            }
        }
    }

    // then params
    if (functionParams != nullptr) {
        for (std::size_t paramIndex = 0; paramIndex < functionParams->size(); ++paramIndex) {
            if ((*functionParams)[paramIndex]->name() == identifierExpr->name()) {
                if (identifierExpr->hasTemplateArguments()) {
                    printError("parameter references cannot have template arguments!",
                               identifierExpr->startPosition(), identifierExpr->endPosition());
                }

                // `paramIndex` is shifted by one if `currentStruct` isn't null. This is because `0` is implicitly the `this` parameter
                Expr* refParam = new RefParameterExpr(identifierExpr->startPosition(),
                                                      identifierExpr->endPosition(),
                                                      paramIndex + (currentStruct ? 1 : 0));
                refParam->resultType = (*functionParams)[paramIndex]->type->deepCopy();
                // A parameter reference is an lvalue.
                refParam->resultType->setIsLValue(true);
                // We always dereference local variable calls if they are references...
                dereferenceReferences(refParam);

                delete identifierExpr;

                expr = refParam;

                return;
            }
        }
    }

    // then function template params
    if (functionTemplateParams != nullptr) {
        for (const TemplateParameterDecl* templateParam : *functionTemplateParams) {
            // TODO: Check if the type of `templateParam` is `FunctionPointerType`
            if (templateParam->name() == identifierExpr->name()) {
                identifierExpr->resultType = templateParam->type->deepCopy();
                return;
            }
        }
    }

//    RefGlobalVariableExpr* foundGlobalVariable = nullptr;
    FunctionDecl* foundFunction = nullptr;
    // `isExactMatch` is used to check for ambiguity
    bool isExactMatch = false;
    bool isAmbiguous = false;
    // TODO: We need to also support function qualifier overloading (`const`, `mut`, `immut` like `int test() const {}`)

    bool hasTemplateArgs = identifierExpr->hasTemplateArguments();

    // then class/struct members
    if (currentStruct) {
        // First declared check members
        for (Decl* member : currentStruct->members) {
            if (member->name() == identifierExpr->name()) {
                if (processIdentifierExprForDecl(member, identifierExpr, hasTemplateArgs,
                                                 foundFunction, isExactMatch, isAmbiguous)) {
                    if (identifierExpr->hasTemplateArguments()) {
                        printError("member variable references cannot have template arguments!",
                                   identifierExpr->startPosition(), identifierExpr->endPosition());
                    }

                    auto foundGlobalVariable = llvm::dyn_cast<GlobalVariableDecl>(member);

                    // Ownership of this will be given to `RefStructMemberVariableExpr`. `RefParameterExpr` gets a copy.
                    StructType* structType = new StructType({}, {}, TypeQualifier::None,
                                                            currentStruct->name(), currentStruct);

                    // `this` is ALWAYS defined as parameter index `0` (when a function is a member of a struct/class, obviously `0` isn't `this` to namespace or file functions)
                    Expr* refParam = new RefParameterExpr(identifierExpr->startPosition(),
                                                          identifierExpr->endPosition(),
                                                          0);
                    refParam->resultType = new ReferenceType({}, {}, TypeQualifier::None, structType->deepCopy());
                    // A parameter reference is an lvalue.
                    refParam->resultType->setIsLValue(true);
                    // NOTE: We still make it a `ReferenceType` above then immediately dereference it here so we create the correct
                    //  expressions for dereferencing the `this` parameter in our `CodeGen`
                    dereferenceReferences(refParam);

                    // Ref the variable using the new `this` variable
                    auto refStructVariable = new RefStructMemberVariableExpr(identifierExpr->startPosition(),
                                                                             identifierExpr->endPosition(),
                                                                             refParam,
                                                                             structType, foundGlobalVariable);
                    refStructVariable->resultType = foundGlobalVariable->type->deepCopy();
                    // A global variable reference is an lvalue.
                    refStructVariable->resultType->setIsLValue(true);

                    delete identifierExpr;

                    expr = refStructVariable;

                    return;
                }
            }
        }

        // Then inherited members
        for (Decl* member : currentStruct->inheritedMembers) {
            if (member->name() == identifierExpr->name()) {
                if (processIdentifierExprForDecl(member, identifierExpr, hasTemplateArgs,
                                                 foundFunction, isExactMatch, isAmbiguous)) {
                    if (identifierExpr->hasTemplateArguments()) {
                        printError("member variable references cannot have template arguments!",
                                   identifierExpr->startPosition(), identifierExpr->endPosition());
                    }

                    auto foundGlobalVariable = llvm::dyn_cast<GlobalVariableDecl>(member);

                    // `this` is ALWAYS defined as parameter index `0` (when a function is a member of a struct/class, obviously `0` isn't `this` to namespace or file functions)
                    Expr* refParam = new RefParameterExpr(identifierExpr->startPosition(),
                                                          identifierExpr->endPosition(),
                                                          0);
                    refParam->resultType = new ReferenceType({}, {}, TypeQualifier::None,
                                                             new StructType({}, {}, TypeQualifier::None,
                                                                            currentStruct->name(),
                                                                            currentStruct));
                    // A parameter reference is an lvalue.
                    refParam->resultType->setIsLValue(true);
                    // NOTE: We still make it a `ReferenceType` above then immediately dereference it here so we create the correct
                    //  expressions for dereferencing the `this` parameter in our `CodeGen`
                    dereferenceReferences(refParam);

                    // Ref the variable using the new `this` variable
                    auto refStructVariable = new RefStructMemberVariableExpr(identifierExpr->startPosition(),
                                                                             identifierExpr->endPosition(),
                                                                             refParam,
                                                                             new StructType({}, {}, TypeQualifier::None,
                                                                                            foundGlobalVariable->parentStruct->name(),
                                                                                            foundGlobalVariable->parentStruct),
                                                                             foundGlobalVariable);
                    refStructVariable->resultType = foundGlobalVariable->type->deepCopy();
                    // A global variable reference is an lvalue.
                    refStructVariable->resultType->setIsLValue(true);

                    delete identifierExpr;

                    expr = refStructVariable;

                    return;
                }
            }
        }

        // TODO: This needs removed
        for (GlobalVariableDecl* memberVariable : currentStruct->dataMembers) {
            if (memberVariable->name() == identifierExpr->name()) {
                if (identifierExpr->hasTemplateArguments()) {
                    printError("member variable references cannot have template arguments!",
                               identifierExpr->startPosition(), identifierExpr->endPosition());
                }

                // Ownership of this will be given to `RefStructMemberVariableExpr`. `RefParameterExpr` gets a copy.
                StructType* structType = new StructType({}, {}, TypeQualifier::None, currentStruct->name(), currentStruct);

                // `this` is ALWAYS defined as parameter index `0` (when a function is a member of a struct/class, obviously `0` isn't `this` to namespace or file functions)
                Expr* refParam = new RefParameterExpr(identifierExpr->startPosition(),
                                                      identifierExpr->endPosition(),
                                                      0);
                refParam->resultType = new ReferenceType({}, {}, TypeQualifier::None, structType->deepCopy());
                // A parameter reference is an lvalue.
                refParam->resultType->setIsLValue(true);
                // NOTE: We still make it a `ReferenceType` above then immediately dereference it here so we create the correct
                //  expressions for dereferencing the `this` parameter in our `CodeGen`
                dereferenceReferences(refParam);

                // Ref the variable using the new `this` variable
                auto refStructVariable = new RefStructMemberVariableExpr(identifierExpr->startPosition(),
                                                                         identifierExpr->endPosition(),
                                                                         refParam,
                                                                         structType, memberVariable);
                refStructVariable->resultType = memberVariable->type->deepCopy();
                // A global variable reference is an lvalue.
                refStructVariable->resultType->setIsLValue(true);

                delete identifierExpr;

                expr = refStructVariable;

                return;
            }
        }
    }

    // TODO: then class/struct template params

    // then current namespace
    if (currentNamespace) {
        for (Decl* decl : currentNamespace->nestedDecls()) {
            if (decl->name() == identifierExpr->name()) {
                if (processIdentifierExprForDecl(decl, identifierExpr, hasTemplateArgs,
                                                 foundFunction, isExactMatch, isAmbiguous)) {
                    auto variableDecl = llvm::dyn_cast<GlobalVariableDecl>(decl);
                    auto globalVariableRef = new RefGlobalVariableExpr(identifierExpr->startPosition(),
                                                                       identifierExpr->endPosition(),
                                                                       variableDecl);

                    globalVariableRef->resultType = variableDecl->type->deepCopy();
                    // A global variable reference is an lvalue.
                    globalVariableRef->resultType->setIsLValue(true);
                    // NOTE: Global variables cannot be references currently
                    //dereferenceReferences(globalVariableRef);
//                delete identifierExpr;
//                expr = globalVariableRef;
//                return;
                    delete expr;
                    expr = globalVariableRef;

                    if (globalVariableRef->globalVariable()->parentNamespace != nullptr) {
                        currentFileAst->addImportExtern(globalVariableRef->globalVariable());
                    }

                    return;
                }
            }
        }
    }

    // then current file
    for (Decl* decl : currentFileAst->topLevelDecls()) {
        if (decl->name() == identifierExpr->name()) {
            if (processIdentifierExprForDecl(decl, identifierExpr, hasTemplateArgs,
                                             foundFunction, isExactMatch, isAmbiguous)) {
                auto variableDecl = llvm::dyn_cast<GlobalVariableDecl>(decl);
                auto globalVariableRef = new RefGlobalVariableExpr(identifierExpr->startPosition(),
                                                                   identifierExpr->endPosition(),
                                                                   variableDecl);

                globalVariableRef->resultType = variableDecl->type->deepCopy();
                // A global variable reference is an lvalue.
                globalVariableRef->resultType->setIsLValue(true);
                // NOTE: Global variables cannot be references currently
                //dereferenceReferences(globalVariableRef);
//                delete identifierExpr;
//                expr = globalVariableRef;
//                return;
                delete expr;
                expr = globalVariableRef;

                if (globalVariableRef->globalVariable()->parentNamespace != nullptr) {
                    currentFileAst->addImportExtern(globalVariableRef->globalVariable());
                }

                return;
            }
        }
    }

    // then imports.
    if (currentImports) {
        for (Import* anImport : *currentImports) {
            for (Decl* decl : anImport->pointToNamespace->nestedDecls()) {
                if (decl->name() == identifierExpr->name()) {
                    if (processIdentifierExprForDecl(decl, identifierExpr, hasTemplateArgs,
                                                     foundFunction, isExactMatch, isAmbiguous)) {
                        auto variableDecl = llvm::dyn_cast<GlobalVariableDecl>(decl);
                        auto globalVariableRef = new RefGlobalVariableExpr(identifierExpr->startPosition(),
                                                                           identifierExpr->endPosition(),
                                                                           variableDecl);

                        globalVariableRef->resultType = variableDecl->type->deepCopy();
                        // A global variable reference is an lvalue.
                        globalVariableRef->resultType->setIsLValue(true);
                        // NOTE: Global variables cannot be references currently
                        //dereferenceReferences(globalVariableRef);
//                delete identifierExpr;
//                expr = globalVariableRef;
//                return;
                        delete expr;
                        expr = globalVariableRef;

                        if (globalVariableRef->globalVariable()->parentNamespace != nullptr) {
                            currentFileAst->addImportExtern(globalVariableRef->globalVariable());
                        }

                        return;
                    }
                }
            }
        }
    }

    if (foundFunction) {
        // If the found function is ambiguous then we have to error, it means there are multiple functions that all require some type of implicit cast to work...
        if (isAmbiguous) {
            printError("function call is ambiguous!",
                       identifierExpr->startPosition(), identifierExpr->endPosition());
        }

        Type* resultTypeCopy = foundFunction->resultType->deepCopy();
        std::vector<Type*> paramTypeCopy{};

        for (const ParameterDecl* param : foundFunction->parameters) {
            paramTypeCopy.push_back(param->type->deepCopy());
        }

        // TODO: the tempalte arguments need to be processed
        Expr* refFileFunc = new RefFunctionExpr(identifierExpr->startPosition(), identifierExpr->endPosition(),
                                                foundFunction);
        refFileFunc->resultType = new FunctionPointerType({}, {}, TypeQualifier::None, resultTypeCopy, paramTypeCopy);

        delete identifierExpr;

        expr = refFileFunc;

        if (foundFunction->parentNamespace != nullptr) {
            currentFileAst->addImportExtern(foundFunction);
        }

        return;
    }

    // if we've made it to this point the identifier cannot be found, error and tell the user.
    printError("identifier '" + identifierExpr->name() + "' was not found in the current context!",
               identifierExpr->startPosition(), identifierExpr->endPosition());
}

void DeclResolver::processImplicitCastExpr(ImplicitCastExpr *implicitCastExpr) {
    // Convert any template type references to the supplied type
    applyTemplateTypeArguments(implicitCastExpr->castType);

    if (implicitCastExpr->resultType == nullptr) {
        implicitCastExpr->resultType = implicitCastExpr->castType->deepCopy();
    } else {
        // If the result type isn't null then we need to make sure the type is converted away from a template type...
        // Convert any template type references to the supplied type
        applyTemplateTypeArguments(implicitCastExpr->resultType);
    }
}

void DeclResolver::processIndexerCallExpr(Expr*& expr) {
    auto indexerCallExpr = llvm::dyn_cast<IndexerCallExpr>(expr);

    // TODO: Support overloading the indexer operator []
    processExpr(indexerCallExpr->indexerReference);

    for (Expr*& argument : indexerCallExpr->arguments) {
        processExpr(argument);
    }

    auto refType = indexerCallExpr->indexerReference->resultType;

    if (llvm::isa<StructType>(refType)) {
        auto structType = llvm::dyn_cast<StructType>(refType);
        auto structDecl = structType->decl();

        IndexOperatorDecl* foundIndexOperator = nullptr;
        // `isExactMatch` is used to check for ambiguity
        bool isExactMatch = false;
        bool isAmbiguous = false;

        for (Decl* decl : structDecl->members) {
            // Skip any decls not visible to us
            if (!VisibilityChecker::canAccessStructMember(structType, currentStruct,
                                                          decl)) {
                continue;
            }

            if (llvm::isa<IndexOperatorDecl>(decl)) {
                auto checkIndexOperator = llvm::dyn_cast<IndexOperatorDecl>(decl);

                if (!checkConstructorOrFunctionMatchesCall(foundIndexOperator, checkIndexOperator,
                                                           &indexerCallExpr->arguments,
                                                           &isExactMatch, &isAmbiguous)) {
                    printError("indexer call is ambiguous!",
                               indexerCallExpr->startPosition(), indexerCallExpr->endPosition());
                }
            }
        }

        if (foundIndexOperator == nullptr) {
            for (Decl* decl : structDecl->inheritedMembers) {
                // Skip any decls not visible to us
                if (!VisibilityChecker::canAccessStructMember(structType, currentStruct,
                                                              decl)) {
                    continue;
                }

                if (llvm::isa<IndexOperatorDecl>(decl)) {
                    auto checkIndexOperator = llvm::dyn_cast<IndexOperatorDecl>(decl);

                    if (!checkConstructorOrFunctionMatchesCall(foundIndexOperator, checkIndexOperator,
                                                               &indexerCallExpr->arguments,
                                                               &isExactMatch, &isAmbiguous)) {
                        printError("indexer call is ambiguous!",
                                   indexerCallExpr->startPosition(), indexerCallExpr->endPosition());
                    }
                }
            }
        }

        if (foundIndexOperator != nullptr) {
            if (isAmbiguous) {
                printError("indexer call is ambiguous!",
                           indexerCallExpr->startPosition(), indexerCallExpr->endPosition());
            }

            if (!isExactMatch) {
                // TODO: Implicitly cast and extract default values from the constructor parameter list
            }

            currentFileAst->addImportExtern(foundIndexOperator);

            auto newExpr = new CustomIndexOperatorCallExpr(expr->startPosition(), expr->endPosition(),
                                                           structType->doVTableCalls() && foundIndexOperator->isVirtual(),
                                                           foundIndexOperator,
                                                           indexerCallExpr->indexerReference,
                                                           std::move(indexerCallExpr->arguments));

            newExpr->resultType = foundIndexOperator->resultType->deepCopy();
            newExpr->resultType->setIsLValue(true);

            // We steal this pointer
            indexerCallExpr->indexerReference = nullptr;

            // Replace the indexer with our custom index operator call
            delete indexerCallExpr;
            expr = newExpr;

            for (std::size_t i = 0; i < foundIndexOperator->parameters.size(); ++i) {
                if (!getTypeIsReference(foundIndexOperator->parameters[i]->type)) {
                    dereferenceReferences(newExpr->arguments[i]);
                    convertLValueToRValue(newExpr->arguments[i]);
                }
            }

            return;
        }

        printError("struct type `" + refType->getString() + "` does not have an indexer operator matching the provided arguments!",
                   indexerCallExpr->startPosition(), indexerCallExpr->endPosition());
    } else {
        printError("type `" + refType->getString() + "` does not have an indexer operator!",
                   indexerCallExpr->startPosition(), indexerCallExpr->endPosition());
    }
}

void DeclResolver::processIntegerLiteralExpr(IntegerLiteralExpr *integerLiteralExpr) {
    // TODO: Type suffix support
    integerLiteralExpr->resultType = (new BuiltInType({}, {}, TypeQualifier::None, "int"));
}

void DeclResolver::processInfixMacroCallExpr(Expr*& expr) {
    auto infixMacroCallExpr = llvm::dyn_cast<InfixMacroCallExpr>(expr);

    processExpr(infixMacroCallExpr->leftValue);
    processExpr(infixMacroCallExpr->rightValue);

    auto leftType = infixMacroCallExpr->leftValue->resultType;
    auto rightType = infixMacroCallExpr->rightValue->resultType;

    // TODO: Once we support actual macros we'll have to search for them too
    if (!llvm::isa<StructType>(leftType)) {
        printError("custom infix operators currently only support structs!",
                   expr->startPosition(), expr->endPosition());
    }

    auto structType = llvm::dyn_cast<StructType>(leftType);
    auto structDecl = structType->decl();

    std::vector<Expr*> operatorArgs {
            infixMacroCallExpr->rightValue
    };

    OperatorDecl* foundOperator = findInfixOperatorOrError(structDecl, &operatorArgs,
                                                           infixMacroCallExpr->macroName(),
                                                           infixMacroCallExpr->startPosition(),
                                                           infixMacroCallExpr->endPosition());

    // TODO: We need to check if `rightValue` needs referenced, dereferenced, or implicitly casted...
    if (foundOperator == nullptr) {
        printError("infix operator `" + infixMacroCallExpr->macroName() + "` not found in struct `" +
                   structDecl->name() + "`!",
                   expr->startPosition(), expr->endPosition());
        return;
    }

    auto newResult = new CustomInfixOperatorCallExpr(expr->startPosition(), expr->endPosition(),
                                                structType->doVTableCalls() && foundOperator->isVirtual(),
                                                     foundOperator,
                                                     infixMacroCallExpr->leftValue,
                                                     infixMacroCallExpr->rightValue);

    newResult->resultType = foundOperator->resultType->deepCopy();
    newResult->resultType->setIsLValue(true);

    infixMacroCallExpr->leftValue = nullptr;
    infixMacroCallExpr->rightValue = nullptr;
    delete infixMacroCallExpr;

    expr = newResult;
}

void DeclResolver::processLocalVariableDeclExpr(LocalVariableDeclExpr *localVariableDeclExpr, bool hasInitialValue) {
    // We process the type as an Expr so that we can resolve any potentially unresolved types...
    processExpr(localVariableDeclExpr->type);

    if (llvm::isa<ResolvedTypeRefExpr>(localVariableDeclExpr->type)) {
        if (localVariableNameTaken(localVariableDeclExpr->name())) {
            printError("redefinition of variable '" + localVariableDeclExpr->name() + "' not allowed!",
                       localVariableDeclExpr->startPosition(),
                       localVariableDeclExpr->endPosition());
        }

        auto resolvedTypeRefExpr = llvm::dyn_cast<ResolvedTypeRefExpr>(localVariableDeclExpr->type);

        // Convert any template type references to the supplied type
        applyTemplateTypeArguments(resolvedTypeRefExpr->resolvedType);

        localVariableDeclExpr->resultType = resolvedTypeRefExpr->resolvedType->deepCopy();

        for (Expr*& initializerArg : localVariableDeclExpr->initializerArgs) {
            processExpr(initializerArg);
        }

        // Find the correct constructor or verify a correct constructor exists...
        // NOTE: We only check the constructor here if they provide initializer arguments or there isn't an initial
        // value
        // TODO: Once we support move and copy constructors we should use `hasInitialValue` to handle move and copy
        if (llvm::isa<StructType>(resolvedTypeRefExpr->resolvedType)) {
            if (localVariableDeclExpr->hasInitializer() || !hasInitialValue) {
                auto structType = llvm::dyn_cast<StructType>(resolvedTypeRefExpr->resolvedType);

                ConstructorDecl *foundConstructor = nullptr;
                // `isExactMatch` is used to check for ambiguity
                bool isExactMatch = false;
                bool isAmbiguous = false;

                for (ConstructorDecl *constructorDecl : structType->decl()->constructors) {
                    // Skip any constructors not visible to us
                    if (!VisibilityChecker::canAccessStructMember(structType, currentStruct,
                                                                  constructorDecl)) {
                        continue;
                    }

                    if (!checkConstructorOrFunctionMatchesCall(foundConstructor, constructorDecl,
                                                               &localVariableDeclExpr->initializerArgs,
                                                               &isExactMatch, &isAmbiguous)) {
                        printError("initializer constructor call is ambiguous!",
                                   localVariableDeclExpr->startPosition(), localVariableDeclExpr->endPosition());
                    }
                }

                if (foundConstructor) {
                    if (isAmbiguous) {
                        printError("initializer constructor call is ambiguous!",
                                   localVariableDeclExpr->startPosition(), localVariableDeclExpr->endPosition());
                    }

                    if (!isExactMatch) {
                        // TODO: Implicitly cast and extract default values from the constructor parameter list
                    }

                    currentFileAst->addImportExtern(foundConstructor);
                    localVariableDeclExpr->foundConstructor = foundConstructor;

                    // Since we found a found a constructor we have to convert any lvalues to rvalues and
                    // dereference references where it makes sense...
                    for (std::size_t i = 0; i < foundConstructor->parameters.size(); ++i) {
                        if (!getTypeIsReference(foundConstructor->parameters[i]->type)) {
                            dereferenceReferences(localVariableDeclExpr->initializerArgs[i]);
                            convertLValueToRValue(localVariableDeclExpr->initializerArgs[i]);
                        }
                    }
                // If a constructor wasn't found but there was a provided initializer then we error saying a
                // constructor wasn't found
                // I.e. `ExampleStruct es;` is legal regardless of if there is an empty constructor or not
                // while `ExampleStruct es(1, 1.0);` MUST have a constructor that can match the provided arguments
                } else if (localVariableDeclExpr->hasInitializer()) {
                    printError("no valid public constructor found for the provided initializer arguments!",
                               localVariableDeclExpr->startPosition(), localVariableDeclExpr->endPosition());
                }
            } else if (localVariableDeclExpr->initializerArgs.size() > 1) {
                printError("non-struct variables can only have 1 initializer argument! (" +
                           std::to_string(localVariableDeclExpr->initializerArgs.size()) + ")",
                           localVariableDeclExpr->startPosition(), localVariableDeclExpr->endPosition());
            }
        }

        addLocalVariable(localVariableDeclExpr);
    }
}

void DeclResolver::processLocalVariableDeclOrPrefixOperatorCallExpr(Expr *&expr) {
    printError("[INTERNAL] PotentialLocalVariableDecl found in declaration resolver!",
               expr->startPosition(), expr->endPosition());
}

bool DeclResolver::attemptAccessStructMember(MemberAccessCallExpr* memberAccessCallExpr, Expr*& outExpr,
                                             bool hasTemplateArgs, StructType* originalStructType, Decl* checkMember,
                                             FunctionDecl*& foundFunction, bool& isExactMatch, bool& isAmbiguous) {
    // Skip anything we cannot access
    if (!VisibilityChecker::canAccessStructMember(originalStructType, currentStruct,
                                                  checkMember)) {
        return false;
    }

    // TODO: Needs to be changed to `MemberVariableDecl`
    if (llvm::isa<GlobalVariableDecl>(checkMember)) {
        auto memberVariable = llvm::dyn_cast<GlobalVariableDecl>(checkMember);
        auto accessAsStructType = new StructType({}, {}, TypeQualifier::None, checkMember->parentStruct->name(), checkMember->parentStruct);

        auto refStructVariable = new RefStructMemberVariableExpr(memberAccessCallExpr->startPosition(),
                                                                 memberAccessCallExpr->endPosition(),
                                                                 memberAccessCallExpr->objectRef,
                                                                 accessAsStructType,
                                                                 memberVariable);
        refStructVariable->resultType = memberVariable->type->deepCopy();
        // A global variable reference is an lvalue.
        refStructVariable->resultType->setIsLValue(true);

        // We steal the object reference
        memberAccessCallExpr->objectRef = nullptr;
        delete memberAccessCallExpr;

        outExpr = refStructVariable;

        return true;
    } else if (llvm::isa<FunctionDecl>(checkMember)) {
        auto function = llvm::dyn_cast<FunctionDecl>(checkMember);

        if (!checkFunctionMatchesCall(foundFunction, function, &isExactMatch, &isAmbiguous)) {
            printError("struct function call is ambiguous!",
                       memberAccessCallExpr->startPosition(), memberAccessCallExpr->endPosition());
        }
    } else if (llvm::isa<TemplateFunctionDecl>(checkMember)) {
        // TODO: We should support implicit template typing (i.e. `int test<T>(T p);` calling `test(12);` == `test<int>(12);`
        if (!hasTemplateArgs) return false;

        auto templateFunctionDecl = llvm::dyn_cast<TemplateFunctionDecl>(checkMember);

        if (templateFunctionDecl->name() == memberAccessCallExpr->member->name()) {
            if (!checkTemplateFunctionMatchesCall(foundFunction, templateFunctionDecl, &isExactMatch, &isAmbiguous, memberAccessCallExpr->member->templateArguments)) {
                printError("struct function call is ambiguous!",
                           memberAccessCallExpr->member->startPosition(), memberAccessCallExpr->member->endPosition());
            }
        }
    } else {
        printError("unknown struct member access call!",
                   memberAccessCallExpr->startPosition(), memberAccessCallExpr->endPosition());
    }

    return false;
}

void DeclResolver::processMemberAccessCallExpr(Expr*& expr) {
    auto memberAccessCallExpr = llvm::dyn_cast<MemberAccessCallExpr>(expr);

    if (memberAccessCallExpr->member->hasTemplateArguments()) {
        for (Expr*& templateArg : memberAccessCallExpr->member->templateArguments) {
            processExpr(templateArg);
        }
    }

    bool hasTemplateArgs = memberAccessCallExpr->member->hasTemplateArguments();

    // TODO: Support operator overloading the `.` and `->` operators
    // TODO: `public override T operator.() => return this.whatever_T_is;` this can ONLY be supported when the implementing class/struct has NO public facing functions
//    processExpr(memberAccessCallExpr->objectRef);
//    processIdentifierExpr(memberAccessCallExpr->member);
    if (llvm::isa<TempNamespaceRefExpr>(memberAccessCallExpr->objectRef)) {
        auto namespaceRef = llvm::dyn_cast<TempNamespaceRefExpr>(memberAccessCallExpr->objectRef);

        FunctionDecl* foundFunction = nullptr;
        bool isExactMatch = false;
        bool isAmbiguous = false;

        // TODO: Should we use the function `processIdentifierExprForDecl` here?
        for (Decl* checkDecl : namespaceRef->namespaceDecl()->nestedDecls()) {
            if (checkDecl->name() == memberAccessCallExpr->member->name()) {
                if (llvm::isa<GlobalVariableDecl>(checkDecl)) {
                    auto globalVariable = llvm::dyn_cast<GlobalVariableDecl>(checkDecl);

                    currentFileAst->addImportExtern(checkDecl);

                    auto refNamespaceVariable = new RefGlobalVariableExpr(memberAccessCallExpr->startPosition(),
                                                                          memberAccessCallExpr->endPosition(),
                                                                          globalVariable);
                    refNamespaceVariable->resultType = globalVariable->type->deepCopy();
                    // A global variable reference is an lvalue.
                    refNamespaceVariable->resultType->setIsLValue(true);

                    delete memberAccessCallExpr;

                    expr = refNamespaceVariable;

                    return;
                } else if (llvm::isa<FunctionDecl>(checkDecl)) {
                    auto function = llvm::dyn_cast<FunctionDecl>(checkDecl);

                    if (!checkFunctionMatchesCall(foundFunction, function, &isExactMatch, &isAmbiguous)) {
                        printError("namespace function call is ambiguous!",
                                   memberAccessCallExpr->startPosition(), memberAccessCallExpr->endPosition());
                    }
                } else if (llvm::isa<TemplateFunctionDecl>(checkDecl)) {
                    // TODO: We should support implicit template typing (i.e. `int test<T>(T p);` calling `test(12);` == `test<int>(12);`
                    if (!hasTemplateArgs) continue;

                    auto templateFunctionDecl = llvm::dyn_cast<TemplateFunctionDecl>(checkDecl);

                    if (templateFunctionDecl->name() == memberAccessCallExpr->member->name()) {
                        if (!checkTemplateFunctionMatchesCall(foundFunction, templateFunctionDecl, &isExactMatch, &isAmbiguous, memberAccessCallExpr->member->templateArguments)) {
                            printError("namespace function call is ambiguous!",
                                       memberAccessCallExpr->member->startPosition(), memberAccessCallExpr->member->endPosition());
                        }
                    }
                } else {
                    printError("unknown namespace member access call!",
                               memberAccessCallExpr->startPosition(), memberAccessCallExpr->endPosition());
                    return;
                }
            }
        }

        if (foundFunction) {
            if (isAmbiguous) {
                printError("namespace function call is ambiguous!",
                           memberAccessCallExpr->startPosition(), memberAccessCallExpr->endPosition());
            }

            Type* resultTypeCopy = foundFunction->resultType->deepCopy();
            std::vector<Type*> paramTypeCopy{};

            for (const ParameterDecl* param : foundFunction->parameters) {
                paramTypeCopy.push_back(param->type->deepCopy());
            }

            // Add the referenced function to a list of potentially imported externs
            currentFileAst->addImportExtern(foundFunction);

            // TODO: the tempalte arguments need to be processed
            auto refNamespaceFunction = new RefFunctionExpr(memberAccessCallExpr->startPosition(),
                                                            memberAccessCallExpr->endPosition(),
                                                            foundFunction);
            refNamespaceFunction->resultType = new FunctionPointerType({}, {}, TypeQualifier::None, resultTypeCopy, paramTypeCopy);

            delete expr;

            expr = refNamespaceFunction;

            return;
        }
    } else {
        if (memberAccessCallExpr->isArrowCall()) {
            // Make `objectRef` a dereference then `.`
            Expr* prefixOperatorCall = new PrefixOperatorExpr(memberAccessCallExpr->objectRef->startPosition(),
                                                             memberAccessCallExpr->objectRef->endPosition(),
                                                             "*",
                                                             memberAccessCallExpr->objectRef);
            processPrefixOperatorExpr(prefixOperatorCall);
            memberAccessCallExpr->objectRef = prefixOperatorCall;
        }

        processExpr(memberAccessCallExpr->objectRef);

        if (memberAccessCallExpr->objectRef->resultType != nullptr) {
            // TODO: We need to support extensions that add member functions to ANY type...
            if (llvm::isa<StructType>(memberAccessCallExpr->objectRef->resultType)) {
                auto structType = llvm::dyn_cast<StructType>(memberAccessCallExpr->objectRef->resultType);

                FunctionDecl* foundFunction = nullptr;
                bool isExactMatch = false;
                bool isAmbiguous = false;

                for (Decl* checkDecl : structType->decl()->members) {
                    if (checkDecl->name() == memberAccessCallExpr->member->name()) {
                        if (attemptAccessStructMember(memberAccessCallExpr, expr,
                                                      hasTemplateArgs, structType, checkDecl,
                                                      foundFunction, isExactMatch, isAmbiguous)) {
                            // If it returns true then we only have to return. The above function handles everything
                            // We had to do
                            return;
                        }
                    }
                }

                // We also have to loop through inherited members...
                for (Decl* checkDecl : structType->decl()->inheritedMembers) {
                    if (checkDecl->name() == memberAccessCallExpr->member->name()) {
                        if (attemptAccessStructMember(memberAccessCallExpr, expr,
                                                      hasTemplateArgs, structType, checkDecl,
                                                      foundFunction, isExactMatch, isAmbiguous)) {
                            // If it returns true then we only have to return. The above function handles everything
                            // We had to do
                            return;
                        }
                    }
                }

                if (foundFunction) {
                    if (isAmbiguous) {
                        printError("struct function call is ambiguous!",
                                   memberAccessCallExpr->startPosition(), memberAccessCallExpr->endPosition());
                    }

                    auto accessAsStructType = new StructType({}, {}, TypeQualifier::None,
                                                             foundFunction->parentStruct->name(),
                                                             foundFunction->parentStruct);
                    Type* resultTypeCopy = foundFunction->resultType->deepCopy();
                    std::vector<Type*> paramTypeCopy{};

                    for (const ParameterDecl* param : foundFunction->parameters) {
                        paramTypeCopy.push_back(param->type->deepCopy());
                    }

                    // TODO: the template arguments need to be processed
                    auto refStructFunction = new RefStructMemberFunctionExpr(memberAccessCallExpr->startPosition(),
                                                                             memberAccessCallExpr->endPosition(),
                                                                             memberAccessCallExpr->objectRef,
                                                                             accessAsStructType,
                                                                             foundFunction,
                                                                             structType->doVTableCalls() && foundFunction->isVirtual());
                    // TODO: We should probably replace this with a `MemberFunctionPointerType` or something so the `this` parameter is apart of the type...
                    refStructFunction->resultType = new FunctionPointerType({}, {}, TypeQualifier::None, resultTypeCopy, paramTypeCopy);

                    // We steal the object reference
                    memberAccessCallExpr->objectRef = nullptr;
                    delete expr;

                    expr = refStructFunction;

                    return;
                }

                printError("member '" + memberAccessCallExpr->member->name() + "' was not found in type '" + structType->getString() + "' or is not public!",
                           memberAccessCallExpr->startPosition(), memberAccessCallExpr->endPosition());
            } else {
                if (llvm::isa<PointerType>(memberAccessCallExpr->objectRef->resultType)) {
                    printError("type '" + memberAccessCallExpr->objectRef->resultType->getString() +
                               "' not supported using `.` operator, did you mean `->`?!",
                               memberAccessCallExpr->startPosition(), memberAccessCallExpr->endPosition());
                } else {
                    printError("type '" + memberAccessCallExpr->objectRef->resultType->getString() +
                               "' not supported in member access call!",
                               memberAccessCallExpr->startPosition(), memberAccessCallExpr->endPosition());
                }
            }
        }
    }

    // If we reach this point, something went wrong.
    printError("unknown expression in member access call!",
               memberAccessCallExpr->startPosition(), memberAccessCallExpr->endPosition());
}

void DeclResolver::processParenExpr(ParenExpr *parenExpr) {
    processExpr(parenExpr->containedExpr);

    parenExpr->resultType = parenExpr->containedExpr->resultType->deepCopy();
}

void DeclResolver::processPostfixOperatorExpr(PostfixOperatorExpr *postfixOperatorExpr) {
    // TODO: Support operator overloading type resolution
    processExpr(postfixOperatorExpr->expr);

    postfixOperatorExpr->resultType = postfixOperatorExpr->expr->resultType->deepCopy();
}

void DeclResolver::processPotentialExplicitCastExpr(Expr*& expr) {
    printError("[INTERNAL] PotentialExplicitCastExpr found in declaration resolver",
               expr->startPosition(), expr->endPosition());
}

void DeclResolver::processPrefixMacroCallExpr(Expr *&expr) {
    auto prefixMacroCallExpr = llvm::dyn_cast<PrefixMacroCallExpr>(expr);

    processExpr(prefixMacroCallExpr->expr);

    if (!llvm::isa<StructType>(prefixMacroCallExpr->expr->resultType)) {
        printError("custom prefix operators/macros currently only work on struct types!",
                   prefixMacroCallExpr->startPosition(), prefixMacroCallExpr->endPosition());
    }

    auto structType = llvm::dyn_cast<StructType>(prefixMacroCallExpr->expr->resultType);
    auto structDecl = structType->decl();

    OperatorDecl* foundOperator = findPrefixOperator(structDecl, prefixMacroCallExpr->macroName());

    if (!foundOperator) {
        printError("prefix operator `" + prefixMacroCallExpr->macroName() + "` was not found for struct `" +
                   structDecl->name() + "`!",
                   prefixMacroCallExpr->startPosition(), prefixMacroCallExpr->endPosition());
        return;
    }

    auto customPrefixOperatorCall = new CustomPrefixOperatorCallExpr(expr->startPosition(), expr->endPosition(),
                                                                     structType->doVTableCalls() && foundOperator->isVirtual(),
                                                                     foundOperator,
                                                                     prefixMacroCallExpr->expr);
    customPrefixOperatorCall->resultType = foundOperator->resultType->deepCopy();
    customPrefixOperatorCall->resultType->setIsLValue(true);
    // We steal this pointer
    prefixMacroCallExpr->expr = nullptr;
    delete prefixMacroCallExpr;

    expr = customPrefixOperatorCall;
}

void DeclResolver::processPrefixOperatorExpr(Expr*& expr) {
    auto prefixOperatorExpr = llvm::dyn_cast<PrefixOperatorExpr>(expr);

    processExpr(prefixOperatorExpr->expr);

    gulc::Type* checkType = prefixOperatorExpr->expr->resultType;

    if (llvm::isa<StructType>(checkType)) {
        auto structType = llvm::dyn_cast<StructType>(checkType);
        auto structDecl = structType->decl();

        OperatorDecl* foundOperator = findPrefixOperator(structDecl, prefixOperatorExpr->operatorName());

        if (!foundOperator) {
            printError("prefix operator `" + prefixOperatorExpr->operatorName() + "` was not found for struct `" +
                       structDecl->name() + "`!",
                       prefixOperatorExpr->startPosition(), prefixOperatorExpr->endPosition());
            return;
        }

        auto customPrefixOperatorCall = new CustomPrefixOperatorCallExpr(expr->startPosition(), expr->endPosition(),
                                                                         structType->doVTableCalls() && foundOperator->isVirtual(),
                                                                         foundOperator,
                                                                         prefixOperatorExpr->expr);
        customPrefixOperatorCall->resultType = foundOperator->resultType->deepCopy();
        customPrefixOperatorCall->resultType->setIsLValue(true);
        // We steal this pointer
        prefixOperatorExpr->expr = nullptr;
        delete prefixOperatorExpr;

        expr = customPrefixOperatorCall;
    } else {
        // TODO: Support operator overloading type resolution
        // TODO: Support `sizeof`, `alignof`, `offsetof`, and `nameof`
        if (prefixOperatorExpr->operatorName() == "&") { // Address
            prefixOperatorExpr->resultType = new PointerType({}, {}, TypeQualifier::None,
                                                             prefixOperatorExpr->expr->resultType->deepCopy());
        } else if (prefixOperatorExpr->operatorName() == "*") { // Dereference
            if (llvm::isa<PointerType>(checkType)) {
                auto pointerType = llvm::dyn_cast<PointerType>(checkType);
                prefixOperatorExpr->resultType = pointerType->pointToType->deepCopy();
                // A dereferenced value is an lvalue
                prefixOperatorExpr->resultType->setIsLValue(true);
            } else {
                printError("cannot dereference non-pointer type `" +
                           prefixOperatorExpr->expr->resultType->getString() + "`!",
                           prefixOperatorExpr->startPosition(), prefixOperatorExpr->endPosition());
                return;
            }
        } else if (prefixOperatorExpr->operatorName() == ".ref") {
            if (!(llvm::isa<RefLocalVariableExpr>(prefixOperatorExpr->expr) ||
                  llvm::isa<RefParameterExpr>(prefixOperatorExpr->expr))) {
                printError("cannot create reference to non-variable call expressions!",
                           prefixOperatorExpr->expr->startPosition(), prefixOperatorExpr->expr->endPosition());
                return;
            }

            prefixOperatorExpr->resultType = new ReferenceType({}, {}, TypeQualifier::None,
                                                               prefixOperatorExpr->expr->resultType->deepCopy());
        } else if (prefixOperatorExpr->operatorName() == ".deref") {
            if (llvm::isa<ReferenceType>(checkType)) {
                auto referenceType = llvm::dyn_cast<ReferenceType>(checkType);
                prefixOperatorExpr->resultType = referenceType->referenceToType->deepCopy();
                // A dereferenced value is an lvalue
                prefixOperatorExpr->resultType->setIsLValue(true);
            } else {
                printError("[INTERNAL] expected reference type!",
                           prefixOperatorExpr->expr->startPosition(), prefixOperatorExpr->expr->endPosition());
                return;
            }
        } else if (prefixOperatorExpr->operatorName() == "--" || prefixOperatorExpr->operatorName() == "++") {
            prefixOperatorExpr->resultType = prefixOperatorExpr->expr->resultType->deepCopy();
        } else {
            // TODO: Is this correct? I think the only prefix operators that will reach here are `+`, `-`, `!`, and `~`
            convertLValueToRValue(prefixOperatorExpr->expr);

            prefixOperatorExpr->resultType = prefixOperatorExpr->expr->resultType->deepCopy();
        }
    }
}

void DeclResolver::processResolvedTypeRefExpr(ResolvedTypeRefExpr *resolvedTypeRefExpr) {

}

void DeclResolver::processStringLiteralExpr(StringLiteralExpr *stringLiteralExpr) {
    // TODO: Type suffix support
    stringLiteralExpr->resultType = (new PointerType({}, {}, TypeQualifier::None, new BuiltInType({}, {}, TypeQualifier::None, "char")));
}

void DeclResolver::processTernaryExpr(TernaryExpr *ternaryExpr) {
    processExpr(ternaryExpr->condition);
    // TODO: Verify both expressions are the same type
    processExpr(ternaryExpr->trueExpr);
    processExpr(ternaryExpr->falseExpr);
}

void DeclResolver::processUnresolvedTypeRefExpr(Expr *&expr) {
    printError("[INTERNAL] UnresolvedTypeRefExpr found in declaration resolver!",
               expr->startPosition(), expr->endPosition());
}

void DeclResolver::dereferenceReferences(Expr*& potentialReference) {
    Type* checkType = potentialReference->resultType;

    // For references we convert the reference using the prefix operator `.deref`
    if (llvm::isa<ReferenceType>(checkType)) {
        auto referenceType = llvm::dyn_cast<ReferenceType>(checkType);

        if (referenceType->isLValue()) {
            Expr* newPotentialReference = new PrefixOperatorExpr(potentialReference->startPosition(),
                                                                 potentialReference->endPosition(),
                                                                 ".deref", potentialReference);
            processPrefixOperatorExpr(newPotentialReference);
            //newPotentialReference->resultType->setIsLValue(potentialReference->resultType->isLValue());
            potentialReference = newPotentialReference;
        } else {
            // TODO: This seems VERY hacky since the only `ReferenceType` with `isLValue` being false are function calls...
            potentialReference->resultType = referenceType->referenceToType;
            potentialReference->resultType->setIsLValue(true);
            referenceType->referenceToType = nullptr;
            delete referenceType;
        }
    }
}

void DeclResolver::convertLValueToRValue(Expr*& potentialLValue) {
    if (potentialLValue->resultType->isLValue()) {
        Expr *newRValue = new LValueToRValueExpr(potentialLValue->startPosition(), potentialLValue->endPosition(),
                                                 potentialLValue);
        newRValue->resultType = potentialLValue->resultType->deepCopy();
        newRValue->resultType->setIsLValue(false);
        potentialLValue = newRValue;
    }
}

unsigned int DeclResolver::applyTemplateTypeArguments(Type*& type) {
    if (llvm::isa<FunctionTemplateTypenameRefType>(type)) {
        auto templateTypenameRef = llvm::dyn_cast<FunctionTemplateTypenameRefType>(type);

        std::size_t paramIndex = templateTypenameRef->templateParameterIndex();
        TemplateParameterDecl* checkParam = (*functionTemplateParams)[paramIndex];

        if (llvm::isa<TemplateTypenameType>(checkParam->type)) {
            const Expr* hopefullyResolvedTypeRef = nullptr;

            if (paramIndex >= functionTemplateArgs->size()) {
                if (!checkParam->hasDefaultArgument()) {
                    printDebugWarning("function template param doesn't have required default argument!");
                    return 0;
                }

                hopefullyResolvedTypeRef = checkParam->defaultArgument();
            } else {
                hopefullyResolvedTypeRef = (*functionTemplateArgs)[paramIndex];
            }

            if (!llvm::isa<ResolvedTypeRefExpr>(hopefullyResolvedTypeRef)) {
                printDebugWarning("unresolved type ref in template arguments list!");
                return 0;
            }

            auto resolvedTypeRef = llvm::dyn_cast<ResolvedTypeRefExpr>(hopefullyResolvedTypeRef);

            // Get the resolved type, delete old `type`, set `type` to the resolved type, and return.
            Type* resultType = resolvedTypeRef->resolvedType->deepCopy();
            delete type;
            type = resultType;
            // TODO: Once we support template classes we will have to offset the function reference index by the size of the template class parameter list
            // We add one so we can use `0` as a type of `null`
            return paramIndex + 1;
        }
    } else if (llvm::isa<PointerType>(type)) {
        return applyTemplateTypeArguments(llvm::dyn_cast<PointerType>(type)->pointToType);
    } else if (llvm::isa<ReferenceType>(type)) {
        return applyTemplateTypeArguments(llvm::dyn_cast<ReferenceType>(type)->referenceToType);
    }
    // TODO: Whenever we support template types we need to also handle their argument types here...
    return 0;
}

Expr *DeclResolver::applyCast(Type *from, Type *to, Expr *castee, CastOperatorType castType) {
    if (llvm::isa<StructType>(from)) {
        auto structType = llvm::dyn_cast<StructType>(from);
        auto structDecl = structType->decl();

        for (Decl* checkDecl : structDecl->members) {
            if (llvm::isa<CastOperatorDecl>(checkDecl)) {
                auto checkCastOperator = llvm::dyn_cast<CastOperatorDecl>(checkDecl);

                // If `castType` is implicit then we can ONLY check implicit casts
                // If `castType` is explicit we also allow using `implicit` casts
                if (castType == CastOperatorType::Implicit) {
                    if (checkCastOperator->castOperatorType() != CastOperatorType::Implicit) {
                        continue;
                    }
                }

                // TODO: This might not work if `to` is const while result type isn't.
                //       casting from non-const to const is valid, so we need to support it.
                if (TypeComparer::getTypesAreSame(checkCastOperator->resultType, to)) {
                    auto result = new CustomCastOperatorCallExpr(castee->startPosition(),
                                                                 castee->endPosition(),
                                                                 structType->doVTableCalls() && checkCastOperator->isVirtual(),
                                                                 checkCastOperator,
                                                                 castee);
                    result->resultType = to->deepCopy();
                    // Returned values are always lvalues
                    result->resultType->setIsLValue(true);

                    return result;
                }
            }
        }

        // Check the inherited members
        for (Decl* checkDecl : structDecl->inheritedMembers) {
            if (llvm::isa<CastOperatorDecl>(checkDecl)) {
                auto checkCastOperator = llvm::dyn_cast<CastOperatorDecl>(checkDecl);

                // If `castType` is implicit then we can ONLY check implicit casts
                // If `castType` is explicit we also allow using `implicit` casts
                if (castType == CastOperatorType::Implicit) {
                    if (checkCastOperator->castOperatorType() != CastOperatorType::Implicit) {
                        continue;
                    }
                }

                // TODO: This might not work if `to` is const while result type isn't.
                //       casting from non-const to const is valid, so we need to support it.
                if (TypeComparer::getTypesAreSame(checkCastOperator->resultType, to)) {
                    auto result = new CustomCastOperatorCallExpr(castee->startPosition(),
                                                                 castee->endPosition(),
                                                                 structType->doVTableCalls() && checkCastOperator->isVirtual(),
                                                                 checkCastOperator,
                                                                 castee);
                    result->resultType = to->deepCopy();
                    // Returned values are always lvalues
                    result->resultType->setIsLValue(true);

                    return result;
                }
            }
        }

        // Check for upcasting
        if (llvm::isa<StructType>(to)) {
            auto toStructType = llvm::dyn_cast<StructType>(to);
            auto toStructDecl = toStructType->decl();

            for (StructDecl* checkBase = structDecl->baseStruct; checkBase != nullptr; checkBase = checkBase->baseStruct) {
                if (toStructDecl == checkBase) {
                    Expr* result;

                    if (castType == CastOperatorType::Explicit) {
                        result = new ExplicitCastExpr(castee->startPosition(),
                                                      castee->endPosition(),
                                                      to->deepCopy(),
                                                      castee);
                    } else {
                        result = new ImplicitCastExpr(castee->startPosition(),
                                                      castee->endPosition(),
                                                      to->deepCopy(),
                                                      castee);
                    }

                    result->resultType = to->deepCopy();
                    // We retain the `lvalue`-ness of a type when casted.
                    result->resultType->setIsLValue(from->isLValue());

                    return result;
                }
            }
        }

        printError("struct `" + structDecl->name() + "` does not have an implicit cast operator to type `" +
                   to->getString() + "`!", castee->startPosition(), castee->endPosition());
        return nullptr;
    } else {
        // If `from` isn't a struct type we let the code verifier validate the implicit cast...
        // TODO: Once we support extensions we need to check if `from` has an extension casting support
        Expr* result;

        if (castType == CastOperatorType::Explicit) {
            result = new ExplicitCastExpr(castee->startPosition(),
                                          castee->endPosition(),
                                          to->deepCopy(),
                                          castee);
        } else {
            result = new ImplicitCastExpr(castee->startPosition(),
                                          castee->endPosition(),
                                          to->deepCopy(),
                                          castee);
        }

        result->resultType = to->deepCopy();
        // We retain the `lvalue`-ness of a type when casted.
        result->resultType->setIsLValue(from->isLValue());

        return result;
    }
}
