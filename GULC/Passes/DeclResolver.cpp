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
#include <AST/Types/FunctionTemplateTypenameRefType.hpp>
#include <AST/Types/TemplateTypenameType.hpp>
#include <AST/Types/UnresolvedType.hpp>
#include <AST/Types/FunctionPointerType.hpp>
#include <AST/Exprs/LValueToRValueExpr.hpp>
#include <AST/Exprs/UnresolvedTypeRefExpr.hpp>
#include <AST/Exprs/LocalVariableDeclOrPrefixOperatorCallExpr.hpp>
#include <AST/Types/ConstType.hpp>
#include <AST/Types/MutType.hpp>
#include <AST/Types/ImmutType.hpp>
#include <AST/Types/ReferenceType.hpp>
#include <AST/Types/RValueReferenceType.hpp>
#include <AST/Exprs/RefLocalVariableExpr.hpp>
#include <AST/Exprs/RefParameterExpr.hpp>
#include <AST/Exprs/RefFileFunctionExpr.hpp>
#include "DeclResolver.hpp"

using namespace gulc;

void DeclResolver::processFile(FileAST &fileAst) {
    currentFileAst = &fileAst;

    for (Decl* decl : fileAst.topLevelDecls()) {
        processDecl(decl);
    }
}

bool DeclResolver::resolveType(Type *&type) {
    if (type->getTypeKind() == Type::Kind::Unresolved) {
        // TODO: Take 'namespacePath' into consideration. If there is a namespace path we obviously don't have to worry about the template params
        auto unresolvedType = llvm::dyn_cast<UnresolvedType>(type);

        if (BuiltInType::isBuiltInType(unresolvedType->name())) {
            // TODO: Support template overloading. Allow someone to implement `struct int<T> {}` that will be found if there are template arguments
            if (unresolvedType->hasTemplateArguments()) {
                printError("built in types do not support templating!",
                           unresolvedType->startPosition(), unresolvedType->endPosition());
            }

            Type *oldType = type;
            type = new BuiltInType(oldType->startPosition(), oldType->endPosition(), unresolvedType->name());
            delete oldType;
            return true;
        }

        // We check the function templates first...
        // Function template params can't be templated themselves?
        if (!unresolvedType->hasTemplateArguments() && functionTemplateParams) {
            for (TemplateParameterDecl *templateParameterDecl : *functionTemplateParams) {
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
    } else if (type->getTypeKind() == Type::Kind::Const) {
        auto constType = llvm::dyn_cast<ConstType>(type);

        if (llvm::isa<ConstType>(constType->pointToType)) {
            printWarning("duplicate `const` qualifier not needed!",
                         constType->startPosition(), constType->endPosition());
        } else if (llvm::isa<MutType>(constType->pointToType)) {
            printError("`const` and `mut` qualifiers are not mixable!",
                       constType->startPosition(), constType->endPosition());
            return false;
        } else if (llvm::isa<ImmutType>(constType->pointToType)) {
            printError("`const` and `immut` qualifiers are not mixable!",
                       constType->startPosition(), constType->endPosition());
            return false;
        }

        return resolveType(constType->pointToType);
    } else if (type->getTypeKind() == Type::Kind::Mut) {
        auto mutType = llvm::dyn_cast<MutType>(type);

        if (llvm::isa<ConstType>(mutType->pointToType)) {
            printError("`mut` and `const` qualifiers are not mixable!",
                       mutType->startPosition(), mutType->endPosition());
            return false;
        } else if (llvm::isa<MutType>(mutType->pointToType)) {
            printWarning("duplicate `mut` qualifier not needed!",
                         mutType->startPosition(), mutType->endPosition());
        } else if (llvm::isa<ImmutType>(mutType->pointToType)) {
            printError("`mut` and `immut` qualifiers are not mixable!",
                       mutType->startPosition(), mutType->endPosition());
            return false;
        }

        return resolveType(mutType->pointToType);
    } else if (type->getTypeKind() == Type::Kind::Immut) {
        auto immutType = llvm::dyn_cast<ImmutType>(type);

        if (llvm::isa<ConstType>(immutType->pointToType)) {
            printError("`immut` and `const` qualifiers are not mixable!",
                       immutType->startPosition(), immutType->endPosition());
            return false;
        } else if (llvm::isa<MutType>(immutType->pointToType)) {
            printError("`immut` and `mut` qualifiers are not mixable!",
                       immutType->startPosition(), immutType->endPosition());
            return false;
        } else if (llvm::isa<ImmutType>(immutType->pointToType)) {
            printWarning("duplicate `immut` qualifier not needed!",
                         immutType->startPosition(), immutType->endPosition());
        }

        return resolveType(immutType->pointToType);
    } else if (type->getTypeKind() == Type::Kind::Pointer) {
        auto pointerType = llvm::dyn_cast<PointerType>(type);
        return resolveType(pointerType->pointToType);
    } else if (type->getTypeKind() == Type::Kind::Reference) {
        auto referenceType = llvm::dyn_cast<ReferenceType>(type);
        return resolveType(referenceType->referenceToType);
    }

    return false;
}

// TODO: Should we just combine with `getTypeGreaterThan` as a `compareTypes`?
// TODO: `ignoreQualifiers` should only ignore the first qualifiers...
bool DeclResolver::getTypesAreSame(const Type* type1, const Type* type2, bool ignoreQualifiers) {
    if (type1->getTypeKind() == type2->getTypeKind()) {
        switch (type1->getTypeKind()) {
            case Type::Kind::Const: {
                auto constType1 = llvm::dyn_cast<ConstType>(type1);
                auto constType2 = llvm::dyn_cast<ConstType>(type2);

                return getTypesAreSame(constType1->pointToType, constType2->pointToType, ignoreQualifiers);
            }
            case Type::Kind::Mut: {
                auto mutType1 = llvm::dyn_cast<MutType>(type1);
                auto mutType2 = llvm::dyn_cast<MutType>(type2);

                return getTypesAreSame(mutType1->pointToType, mutType2->pointToType, ignoreQualifiers);
            }
            case Type::Kind::Immut: {
                auto immutType1 = llvm::dyn_cast<ImmutType>(type1);
                auto immutType2 = llvm::dyn_cast<ImmutType>(type2);

                return getTypesAreSame(immutType1->pointToType, immutType2->pointToType, ignoreQualifiers);
            }
            case Type::Kind::BuiltIn: {
                auto builtInType1 = llvm::dyn_cast<BuiltInType>(type1);
                auto builtInType2 = llvm::dyn_cast<BuiltInType>(type2);

                return builtInType1->name() == builtInType2->name();
            }
            case Type::Kind::FunctionTemplateTypenameRef: {
                auto pointerType1 = llvm::dyn_cast<PointerType>(type1);
                auto pointerType2 = llvm::dyn_cast<PointerType>(type2);

                return getTypesAreSame(pointerType1->pointToType, pointerType2->pointToType, ignoreQualifiers);
            }
            case Type::Kind::Pointer: {
                auto pointerType1 = llvm::dyn_cast<PointerType>(type1);
                auto pointerType2 = llvm::dyn_cast<PointerType>(type2);

                return getTypesAreSame(pointerType1->pointToType, pointerType2->pointToType, ignoreQualifiers);
            }
            case Type::Kind::Reference: {
                auto refType1 = llvm::dyn_cast<ReferenceType>(type1);
                auto refType2 = llvm::dyn_cast<ReferenceType>(type2);

                return getTypesAreSame(refType1->referenceToType, refType2->referenceToType, ignoreQualifiers);
            }
			case Type::Kind::FunctionPointer: {
				auto funcPointerType1 = llvm::dyn_cast<FunctionPointerType>(type1);
				auto funcPointerType2 = llvm::dyn_cast<FunctionPointerType>(type2);

				if (!getTypesAreSame(funcPointerType1->resultType, funcPointerType2->resultType, ignoreQualifiers)) {
					return false;
				}

				if (funcPointerType1->paramTypes.empty() != funcPointerType2->paramTypes.empty()) {
					return false;
				}

				if (!funcPointerType1->paramTypes.empty()) {
					for (std::size_t i = 0; i < funcPointerType1->paramTypes.size(); ++i) {
						if (!getTypesAreSame(funcPointerType1->paramTypes[i], funcPointerType2->paramTypes[i], ignoreQualifiers)) {
							return false;
						}
					}
				}

				return true;
			}
            case Type::Kind::TemplateTypename: {
                return true;
            }
            case Type::Kind::Unresolved: {
                std::cout << "gulc qualify type pass [DEBUG WARNING]: attempted to compare two 'UnresolvedType's, operation cannot be completed. Defaulting to false." << std::endl;
                return false;
            }
        }
    } else if (ignoreQualifiers) { // Types are not the same...
        bool typeChanged = false;

        if (llvm::isa<MutType>(type1)) {
            type1 = llvm::dyn_cast<MutType>(type1)->pointToType;
            typeChanged = true;
        } else if (llvm::isa<ConstType>(type1)) {
            type1 = llvm::dyn_cast<ConstType>(type1)->pointToType;
            typeChanged = true;
        } else if (llvm::isa<ImmutType>(type1)) {
            type1 = llvm::dyn_cast<ImmutType>(type1)->pointToType;
            typeChanged = true;
        }

        if (llvm::isa<MutType>(type2)) {
            type2 = llvm::dyn_cast<MutType>(type2)->pointToType;
            typeChanged = true;
        } else if (llvm::isa<ConstType>(type2)) {
            type2 = llvm::dyn_cast<ConstType>(type2)->pointToType;
            typeChanged = true;
        } else if (llvm::isa<ImmutType>(type2)) {
            type2 = llvm::dyn_cast<ImmutType>(type2)->pointToType;
            typeChanged = true;
        }

        if (typeChanged) {
            return getTypesAreSame(type1, type2, ignoreQualifiers);
        }
    }

    return false;
}

bool DeclResolver::shouldCastType(const Type* to, const Type* from) {
    return false;
}

bool DeclResolver::getTypeIsReference(const Type* check) {
    if (llvm::isa<MutType>(check)) {
        check = llvm::dyn_cast<MutType>(check)->pointToType;
    } else if (llvm::isa<ConstType>(check)) {
        check = llvm::dyn_cast<ConstType>(check)->pointToType;
    } else if (llvm::isa<ImmutType>(check)) {
        check = llvm::dyn_cast<ImmutType>(check)->pointToType;
    }

    return llvm::isa<ReferenceType>(check) || llvm::isa<RValueReferenceType>(check);
}

Type *DeclResolver::deepCopyAndSimplifyType(const Type *type) {
    switch (type->getTypeKind()) {
        case Type::Kind::BuiltIn: {
            auto builtInType = llvm::dyn_cast<BuiltInType>(type);
            return new BuiltInType(builtInType->startPosition(), builtInType->endPosition(), builtInType->name());
        }
        case Type::Kind::FunctionTemplateTypenameRef: {
            auto functionTemplateTypenameRef = llvm::dyn_cast<FunctionTemplateTypenameRefType>(type);
            return new FunctionTemplateTypenameRefType(functionTemplateTypenameRef->startPosition(),
                                                       functionTemplateTypenameRef->endPosition(),
                                                       functionTemplateTypenameRef->name());
        }
        case Type::Kind::Const: {
            auto constType = llvm::dyn_cast<ConstType>(type);
            return new ConstType(constType->startPosition(), constType->endPosition(), deepCopyAndSimplifyType(constType->pointToType));
        }
        case Type::Kind::Mut: {
            auto mutType = llvm::dyn_cast<MutType>(type);
            return new MutType(mutType->startPosition(), mutType->endPosition(), deepCopyAndSimplifyType(mutType->pointToType));
        }
        case Type::Kind::Immut: {
            auto immutType = llvm::dyn_cast<ImmutType>(type);
            return new ImmutType(immutType->startPosition(), immutType->endPosition(), deepCopyAndSimplifyType(immutType->pointToType));
        }
        case Type::Kind::Pointer: {
            auto pointerType = llvm::dyn_cast<PointerType>(type);
            return new PointerType(pointerType->startPosition(), pointerType->endPosition(), deepCopyAndSimplifyType(pointerType->pointToType));
        }
		case Type::Kind::FunctionPointer: {
			auto functionPointer = llvm::dyn_cast<FunctionPointerType>(type);
			Type* resultType = deepCopyAndSimplifyType(functionPointer->resultType);
			std::vector<Type*> paramTypes{};

			if (!functionPointer->paramTypes.empty()) {
				paramTypes.reserve(functionPointer->paramTypes.size());

				for (const Type* paramType : functionPointer->paramTypes) {
					paramTypes.emplace_back(deepCopyAndSimplifyType(paramType));
				}
			}

			return new FunctionPointerType(functionPointer->startPosition(), functionPointer->endPosition(), resultType, paramTypes);
		}
        case Type::Kind::TemplateTypename: {
            auto templateTypenameType = llvm::dyn_cast<TemplateTypenameType>(type);
            return new TemplateTypenameType(templateTypenameType->startPosition(), templateTypenameType->endPosition());
        }
        case Type::Kind::Reference: {
            auto referenceType = llvm::dyn_cast<ReferenceType>(type);
            return new ReferenceType(referenceType->startPosition(), referenceType->endPosition(), deepCopyAndSimplifyType(referenceType->referenceToType));
        }
        case Type::Kind::RValueReference: {
            auto rvalueReferenceType = llvm::dyn_cast<RValueReferenceType>(type);
            return new ReferenceType(rvalueReferenceType->startPosition(), rvalueReferenceType->endPosition(), deepCopyAndSimplifyType(rvalueReferenceType->referenceToType));
        }
        case Type::Kind::Unresolved: {
            std::cout << "gulc [INTERNAL] resolver error: attempted to deep copy unresolved type, operation not supported!" << std::endl;
            std::exit(1);
        }
		default:
			std::cout << "gulc [INTERNAL] resolver error: attempted to deep copy an unsupported type!" << std::endl;
			std::exit(1);
    }
    // MSVC apparently doesn't take 'std::exit' into account and thinks we don't return on all code paths...
    return nullptr;
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
        case Decl::Kind::Function:
            processFunctionDecl(llvm::dyn_cast<FunctionDecl>(decl));
            break;
        case Decl::Kind::Parameter:
        case Decl::Kind::TemplateParameterDecl:
        default:
            printDebugWarning("unhandled Decl in 'processDecl'!");
            break;
    }
}

void DeclResolver::processStmt(Stmt *&stmt) {
    switch (stmt->getStmtKind()) {
        case Stmt::Kind::Break:
            processBreakStmt(llvm::dyn_cast<BreakStmt>(stmt));
            break;
        case Stmt::Kind::Case:
            processCaseStmt(llvm::dyn_cast<CaseStmt>(stmt));
            break;
        case Stmt::Kind::Compound:
            processCompoundStmt(llvm::dyn_cast<CompoundStmt>(stmt));
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
        case Stmt::Kind::TryCatch:
            processTryCatchStmt(llvm::dyn_cast<TryCatchStmt>(stmt));
            break;
        case Stmt::Kind::TryFinally:
            processTryFinallyStmt(llvm::dyn_cast<TryFinallyStmt>(stmt));
            break;
        case Stmt::Kind::While:
            processWhileStmt(llvm::dyn_cast<WhileStmt>(stmt));
            break;
        case Stmt::Kind::Expr: {
            auto expr = llvm::dyn_cast<Expr>(stmt);
            processExpr(expr);
            stmt = expr;
        }
    }
}

void DeclResolver::processExpr(Expr *&expr) {
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
        case Expr::Kind::LocalVariableDeclOrPrefixOperatorCallExpr:
            // Casting isn't required for this function. It will handle the casting for us since this is a type we will be completely removing from the AST in this function
            processLocalVariableDeclOrPrefixOperatorCallExpr(expr);
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
        case Expr::Kind::PotentialExplicitCast:
            processPotentialExplicitCastExpr(expr);
            break;
        case Expr::Kind::PrefixOperator:
            processPrefixOperatorExpr(llvm::dyn_cast<PrefixOperatorExpr>(expr));
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
    }
}

// Decls
void DeclResolver::processFunctionDecl(FunctionDecl *functionDecl) {
    if (functionDecl->hasTemplateParameters()) {
        bool shouldHaveDefaultArgument = false;

        // Make sure all template parameters after the first optional template parameter are also optional
        for (const TemplateParameterDecl* templateParameterDecl : functionDecl->templateParameters) {
            if (templateParameterDecl->hasDefaultArgument()) {
                if (!shouldHaveDefaultArgument) {
                    shouldHaveDefaultArgument = true;
                } else {
                    printError("all template parameters after the first optional template parameter must also be optional!",
                               templateParameterDecl->startPosition(), templateParameterDecl->endPosition());
                }
            }
        }

        functionTemplateParams = &functionDecl->templateParameters;
    }

    // Resolve function return type...
    if (!resolveType(functionDecl->resultType)) {
        printError("could not find function return type!",
                   functionDecl->resultType->startPosition(), functionDecl->resultType->endPosition());
    }

    if (llvm::isa<ConstType>(functionDecl->resultType)) {
        printWarning("`const` qualifier unneeded on function result type, function return types are already `const` qualified!",
                     functionDecl->startPosition(), functionDecl->endPosition());
    } else if (llvm::isa<MutType>(functionDecl->resultType)) {
        printWarning("`mut` does not apply to function return types, `mut` qualifier is ignored...",
                     functionDecl->startPosition(), functionDecl->endPosition());

        // We remove the `mut` qualifier to try to reduce any confusion...
        auto mutType = llvm::dyn_cast<MutType>(functionDecl->resultType);
        functionDecl->resultType = mutType->pointToType;
        mutType->pointToType = nullptr;
        delete mutType;
    }
    // The result type CAN be `immut` since `immut` has a requirement of NEVER being modified -
    //  unlike `const` which can be modified in situations where the underlying type has a `mut` member variable...

    // Resolve the template parameter types (we allow `void func<typename T, T value>()`)
    for (TemplateParameterDecl*& templateParameterDecl : functionDecl->templateParameters) {
        if (templateParameterDecl->type->getTypeKind() == Type::Kind::Unresolved) {
            if (!resolveType(templateParameterDecl->type)) {
                printError("could not find function template type!",
                           templateParameterDecl->startPosition(), templateParameterDecl->endPosition());
            }
        }
    }

    if (functionDecl->hasParameters()) {
        bool shouldHaveDefaultArgument = false;

        // Make sure all parameters after the first optional parameter are also optional
        for (ParameterDecl* parameterDecl : functionDecl->parameters) {
            if (!resolveType(parameterDecl->type)) {
                printError("could not find function parameter type!",
                           parameterDecl->startPosition(), parameterDecl->endPosition());
            }

            if (parameterDecl->hasDefaultArgument()) {
                if (!shouldHaveDefaultArgument) {
                    shouldHaveDefaultArgument = true;
                } else {
                    printError("all parameters after the first optional parameter must also be optional!",
                               parameterDecl->startPosition(), parameterDecl->endPosition());
                }
            }
        }

        functionParams = &functionDecl->parameters;
    }

    returnType = functionDecl->resultType;

    // We reset to zero just in case.
    functionLocalVariablesCount = 0;
    processCompoundStmt(functionDecl->body());
    functionLocalVariablesCount = 0;

    returnType = nullptr;
    functionParams = nullptr;
    functionTemplateParams = nullptr;
}

// Stmts
void DeclResolver::processBreakStmt(BreakStmt *breakStmt) {
    if (!breakStmt->label().empty()) {
        addUnresolvedLabel(breakStmt->label());
    }
}

void DeclResolver::processCaseStmt(CaseStmt *caseStmt) {
    if (caseStmt->hasCondition()) {
        processExpr(caseStmt->condition);
    }

    processStmt(caseStmt->trueStmt);
}

void DeclResolver::processCompoundStmt(CompoundStmt *compoundStmt) {
    unsigned int oldLocalVariableCount = functionLocalVariablesCount;

    for (Stmt*& stmt : compoundStmt->statements()) {
        processStmt(stmt);
    }

    functionLocalVariablesCount = oldLocalVariableCount;
}

void DeclResolver::processContinueStmt(ContinueStmt *continueStmt) {
    if (!continueStmt->label().empty()) {
        addUnresolvedLabel(continueStmt->label());
    }
}

void DeclResolver::processDoStmt(DoStmt *doStmt) {
    processStmt(doStmt->loopStmt);
    processExpr(doStmt->condition);
}

void DeclResolver::processForStmt(ForStmt *forStmt) {
    if (forStmt->preLoop != nullptr) processExpr(forStmt->preLoop);
    if (forStmt->condition != nullptr) processExpr(forStmt->condition);
    if (forStmt->iterationExpr != nullptr) processExpr(forStmt->iterationExpr);

    processStmt(forStmt->loopStmt);
}

void DeclResolver::processGotoStmt(GotoStmt *gotoStmt) {
    if (!gotoStmt->label().empty()) {
        addUnresolvedLabel(gotoStmt->label());
    }
}

void DeclResolver::processIfStmt(IfStmt *ifStmt) {
    processExpr(ifStmt->condition);
    processStmt(ifStmt->trueStmt);
    if (ifStmt->hasFalseStmt()) processStmt(ifStmt->falseStmt);
}

void DeclResolver::processLabeledStmt(LabeledStmt *labeledStmt) {
    processStmt(labeledStmt->labeledStmt);

    labelResolved(labeledStmt->label());
}

void DeclResolver::processReturnStmt(ReturnStmt *returnStmt) {
    if (returnStmt->hasReturnValue()) {
        processExpr(returnStmt->returnValue);

        bool typesAreSame = false;

        // If the return value isn't a reference but the return type of the function is we need to create a reference? This should probably be illegal?
        if (!getTypeIsReference(returnStmt->returnValue->resultType)) {
            if (llvm::isa<ReferenceType>(returnType)) {
                auto referenceType = llvm::dyn_cast<ReferenceType>(returnType);
                typesAreSame = getTypesAreSame(returnStmt->returnValue->resultType, referenceType->referenceToType, true);
            } else if (llvm::isa<RValueReferenceType>(returnType)) {
                auto referenceType = llvm::dyn_cast<RValueReferenceType>(returnType);
                typesAreSame = getTypesAreSame(returnStmt->returnValue->resultType, referenceType->referenceToType, true);
            } else {
                typesAreSame = getTypesAreSame(returnStmt->returnValue->resultType, returnType, true);
            }
        // If the return type of the function isn't a reference but the result value type is then we dereference the result value
        } else if (!getTypeIsReference(returnType)) {
            if (llvm::isa<ReferenceType>(returnStmt->returnValue->resultType)) {
                auto referenceType = llvm::dyn_cast<ReferenceType>(returnStmt->returnValue->resultType);
                typesAreSame = getTypesAreSame(returnType, referenceType->referenceToType, true);
            } else if (llvm::isa<RValueReferenceType>(returnStmt->returnValue->resultType)) {
                auto referenceType = llvm::dyn_cast<RValueReferenceType>(returnStmt->returnValue->resultType);
                typesAreSame = getTypesAreSame(returnType, referenceType->referenceToType, true);
            } else {
                typesAreSame = getTypesAreSame(returnStmt->returnValue->resultType, returnType, true);
            }
        } else {
            typesAreSame = getTypesAreSame(returnStmt->returnValue->resultType, returnType, true);
        }

        if (!getTypeIsReference(returnType)) {
            dereferenceReferences(returnStmt->returnValue);
            convertLValueToRValue(returnStmt->returnValue);
        }


        if (!typesAreSame) {
            // We will handle this in the verifier.
            auto implicitCastExpr = new ImplicitCastExpr(returnStmt->returnValue->startPosition(),
                                                         returnStmt->returnValue->endPosition(),
                                                         deepCopyAndSimplifyType(returnType),
                                                         returnStmt->returnValue);

            processImplicitCastExpr(implicitCastExpr);

            returnStmt->returnValue = implicitCastExpr;
        }
    }
}

void DeclResolver::processSwitchStmt(SwitchStmt *switchStmt) {
    // TODO: Should we add the type of 'SwitchStmt::condition' to the context?
    processExpr(switchStmt->condition);

    for (CaseStmt* caseStmt : switchStmt->cases()) {
        processCaseStmt(caseStmt);
    }
}

void DeclResolver::processTryStmt(TryStmt *tryStmt) {
    processCompoundStmt(tryStmt->encapsulatedStmt);

    if (tryStmt->hasCatchStmts()) {
        for (TryCatchStmt*& catchStmt : tryStmt->catchStmts()) {
            processTryCatchStmt(catchStmt);
        }
    }

    if (tryStmt->hasFinallyStmt()) {
        processTryFinallyStmt(tryStmt->finallyStmt);
    }
}

void DeclResolver::processTryCatchStmt(TryCatchStmt *tryCatchStmt) {
    if (tryCatchStmt->hasExceptionDecl()) {
        processExpr(tryCatchStmt->exceptionType);
    }

    processCompoundStmt(tryCatchStmt->handlerStmt);
}

void DeclResolver::processTryFinallyStmt(TryFinallyStmt *tryFinallyStmt) {
    processCompoundStmt(tryFinallyStmt->handlerStmt);
}

void DeclResolver::processWhileStmt(WhileStmt *whileStmt) {
    processExpr(whileStmt->condition);
    processStmt(whileStmt->loopStmt);
}

// Exprs
void DeclResolver::processBinaryOperatorExpr(Expr*& expr) {
    auto binaryOperatorExpr = llvm::dyn_cast<BinaryOperatorExpr>(expr);

    // TODO: Support operator overloading type resolution
    processExpr(binaryOperatorExpr->leftValue);

    if (llvm::isa<ResolvedTypeRefExpr>(binaryOperatorExpr->leftValue)) {
        // TODO: Support the other type suffixes
        if (binaryOperatorExpr->operatorName() == "*") {
            if (!llvm::isa<IdentifierExpr>(binaryOperatorExpr->rightValue)) {
                printError("expected local variable name after pointer type!",
                           binaryOperatorExpr->rightValue->startPosition(),
                           binaryOperatorExpr->rightValue->endPosition());
                return;
            }

            auto localVarTypeRef = llvm::dyn_cast<ResolvedTypeRefExpr>(binaryOperatorExpr->leftValue);
            auto varNameExpr = llvm::dyn_cast<IdentifierExpr>(binaryOperatorExpr->rightValue);

            if (varNameExpr->hasTemplateArguments()) {
                printError("local variable name cannot have template arguments!",
                           varNameExpr->startPosition(),
                           varNameExpr->endPosition());
                return;
            }

            binaryOperatorExpr->leftValue = nullptr;

            std::string varName = varNameExpr->name();

            localVarTypeRef->resolvedType = new PointerType(localVarTypeRef->startPosition(),
                                                            localVarTypeRef->endPosition(),
                                                            localVarTypeRef->resolvedType);

            auto localVarDeclExpr = new LocalVariableDeclExpr(binaryOperatorExpr->startPosition(),
                                                              binaryOperatorExpr->endPosition(), localVarTypeRef,
                                                              varName);

            processLocalVariableDeclExpr(localVarDeclExpr);
            expr = localVarDeclExpr;

            // Delete the binary operator expression (this will delete the left and right expressions)
            delete binaryOperatorExpr;

            return;
        } else if (binaryOperatorExpr->operatorName() == "&") {
            if (!llvm::isa<IdentifierExpr>(binaryOperatorExpr->rightValue)) {
                printError("expected local variable name after reference type!",
                           binaryOperatorExpr->rightValue->startPosition(),
                           binaryOperatorExpr->rightValue->endPosition());
                return;
            }

            auto localVarTypeRef = llvm::dyn_cast<ResolvedTypeRefExpr>(binaryOperatorExpr->leftValue);
            auto varNameExpr = llvm::dyn_cast<IdentifierExpr>(binaryOperatorExpr->rightValue);

            if (varNameExpr->hasTemplateArguments()) {
                printError("local variable name cannot have template arguments!",
                           varNameExpr->startPosition(),
                           varNameExpr->endPosition());
                return;
            }

            binaryOperatorExpr->leftValue = nullptr;

            std::string varName = varNameExpr->name();

            localVarTypeRef->resolvedType = new ReferenceType(localVarTypeRef->startPosition(),
                                                            localVarTypeRef->endPosition(),
                                                            localVarTypeRef->resolvedType);

            auto localVarDeclExpr = new LocalVariableDeclExpr(binaryOperatorExpr->startPosition(),
                                                              binaryOperatorExpr->endPosition(), localVarTypeRef,
                                                              varName);

            processLocalVariableDeclExpr(localVarDeclExpr);
            expr = localVarDeclExpr;

            // Delete the binary operator expression (this will delete the left and right expressions)
            delete binaryOperatorExpr;

            return;
        } else if (binaryOperatorExpr->operatorName() == "&&") {
            if (!llvm::isa<IdentifierExpr>(binaryOperatorExpr->rightValue)) {
                printError("expected local variable name after rvalue reference type!",
                           binaryOperatorExpr->rightValue->startPosition(),
                           binaryOperatorExpr->rightValue->endPosition());
                return;
            }

            auto localVarTypeRef = llvm::dyn_cast<ResolvedTypeRefExpr>(binaryOperatorExpr->leftValue);
            auto varNameExpr = llvm::dyn_cast<IdentifierExpr>(binaryOperatorExpr->rightValue);

            if (varNameExpr->hasTemplateArguments()) {
                printError("local variable name cannot have template arguments!",
                           varNameExpr->startPosition(),
                           varNameExpr->endPosition());
                return;
            }

            binaryOperatorExpr->leftValue = nullptr;

            std::string varName = varNameExpr->name();

            localVarTypeRef->resolvedType = new RValueReferenceType(localVarTypeRef->startPosition(),
                                                              localVarTypeRef->endPosition(),
                                                              localVarTypeRef->resolvedType);

            auto localVarDeclExpr = new LocalVariableDeclExpr(binaryOperatorExpr->startPosition(),
                                                              binaryOperatorExpr->endPosition(), localVarTypeRef,
                                                              varName);

            processLocalVariableDeclExpr(localVarDeclExpr);
            expr = localVarDeclExpr;

            // Delete the binary operator expression (this will delete the left and right expressions)
            delete binaryOperatorExpr;

            return;
        } else {
            printError("unsupported binary operator expression with a type as an lvalue!",
                       binaryOperatorExpr->startPosition(),
                       binaryOperatorExpr->endPosition());
            return;
        }
    }

    processExpr(binaryOperatorExpr->rightValue);

    if (binaryOperatorExpr->isBuiltInAssignmentOperator()) {
        Type* leftType = binaryOperatorExpr->leftValue->resultType;
        Type* rightType = binaryOperatorExpr->rightValue->resultType;

        if (getTypeIsReference(leftType)) {
            // If the left type is a reference and the left value is a local variable declaration then we set the left variable to the reference of what is on the right...
            // If the left value ISN'T a local variable then we deref the left value and set it equal to the right value (with the right value being converted to an rvalue)
            if (llvm::isa<LocalVariableDeclExpr>(binaryOperatorExpr->leftValue)) {
                // Right value must be either lvalue or reference
                if (!(rightType->isLValue() || getTypeIsReference(rightType))) {
                    printError("initial value for reference variable MUST be an lvalue!",
                               binaryOperatorExpr->rightValue->startPosition(),
                               binaryOperatorExpr->rightValue->endPosition());
                    return;
                }

                // If the right type isn't already a reference then make it one...
                if (!getTypeIsReference(rightType)) {
                    binaryOperatorExpr->rightValue = new PrefixOperatorExpr(
                            binaryOperatorExpr->rightValue->startPosition(),
                            binaryOperatorExpr->rightValue->endPosition(),
                            ".ref", binaryOperatorExpr->rightValue);
                    processExpr(binaryOperatorExpr->rightValue);
                    rightType = binaryOperatorExpr->rightValue->resultType;
                }
            } else {
                // This will dereference the left value
                dereferenceReferences(binaryOperatorExpr->leftValue);
                leftType = binaryOperatorExpr->leftValue->resultType;

                // The right value here MUST be an rvalue...
                convertLValueToRValue(binaryOperatorExpr->rightValue);
                rightType = binaryOperatorExpr->rightValue->resultType;
            }
        } else {
            // If the left value isn't a reference then the right value has to be an rvalue...
            convertLValueToRValue(binaryOperatorExpr->rightValue);
            rightType = binaryOperatorExpr->rightValue->resultType;
        }

        if (!getTypesAreSame(leftType, rightType, true)) {
            binaryOperatorExpr->rightValue = new ImplicitCastExpr(binaryOperatorExpr->rightValue->startPosition(),
                                                                  binaryOperatorExpr->rightValue->endPosition(),
                                                                  deepCopyAndSimplifyType(leftType),
                                                                  binaryOperatorExpr->rightValue);
        }

        binaryOperatorExpr->resultType = deepCopyAndSimplifyType(leftType);

        if (binaryOperatorExpr->operatorName() != "=") {
            if (llvm::isa<LocalVariableDeclExpr>(binaryOperatorExpr->leftValue)) {
                printError("operator '" + binaryOperatorExpr->operatorName() + "' not supported in variable declaration!",
                           binaryOperatorExpr->startPosition(), binaryOperatorExpr->endPosition());
                return;
            }

            std::string rightOperatorName;

            if (binaryOperatorExpr->operatorName() == ">>=") {
                rightOperatorName = ">>";
            } else if (binaryOperatorExpr->operatorName() == "<<=") {
                rightOperatorName = "<<";
            } else if (binaryOperatorExpr->operatorName() == "+=") {
                rightOperatorName = "+";
            } else if (binaryOperatorExpr->operatorName() == "-=") {
                rightOperatorName = "-";
            } else if (binaryOperatorExpr->operatorName() == "*=") {
                rightOperatorName = "*";
            } else if (binaryOperatorExpr->operatorName() == "/=") {
                rightOperatorName = "/";
            } else if (binaryOperatorExpr->operatorName() == "%=") {
                rightOperatorName = "%";
            } else if (binaryOperatorExpr->operatorName() == "&=") {
                rightOperatorName = "&";
            } else if (binaryOperatorExpr->operatorName() == "|=") {
                rightOperatorName = "|";
            } else if (binaryOperatorExpr->operatorName() == "^=") {
                rightOperatorName = "^";
            } else {
                printError(
                        "[INTERNAL] unknown built in assignment operator '" + binaryOperatorExpr->operatorName() + "'!",
                        binaryOperatorExpr->startPosition(), binaryOperatorExpr->endPosition());
                return;
            }

            Expr* leftValue = binaryOperatorExpr->leftValue;
            convertLValueToRValue(leftValue);
            // We set a new right value with an `LValueToRValueExpr` that doesn't own the pointer. Making it so the left L2R expression doesn't delete the pointer since the binary operator owns it.
            auto newRightValue = new BinaryOperatorExpr(binaryOperatorExpr->startPosition(),
                                                        binaryOperatorExpr->endPosition(),
                                                        rightOperatorName,
                                                        leftValue,
                                                        binaryOperatorExpr->rightValue);

            newRightValue->resultType = deepCopyAndSimplifyType(leftType);

            // Set the new right value and change the operator name to '='
            binaryOperatorExpr->setOperatorName("=");
            binaryOperatorExpr->rightValue = newRightValue;
        }

        return;
    } else {
        convertLValueToRValue(binaryOperatorExpr->leftValue);
        convertLValueToRValue(binaryOperatorExpr->rightValue);

        Type* leftType = binaryOperatorExpr->leftValue->resultType;
        Type* rightType = binaryOperatorExpr->rightValue->resultType;

        if (llvm::isa<BuiltInType>(leftType) && llvm::isa<BuiltInType>(rightType)) {
            auto leftBuiltInType = llvm::dyn_cast<BuiltInType>(leftType);
            auto rightBuiltInType = llvm::dyn_cast<BuiltInType>(rightType);

            bool leftIsSigned = leftBuiltInType->isSigned();
            bool rightIsSigned = rightBuiltInType->isSigned();
            bool leftIsFloating = leftBuiltInType->isFloating();
            bool rightIsFloating = rightBuiltInType->isFloating();

            // If one side is signed and the other isn't then we cast to the signed side.
            if (leftIsSigned) {
                if (!rightIsSigned) {
                    binaryOperatorExpr->leftValue = new ImplicitCastExpr(binaryOperatorExpr->leftValue->startPosition(),
                                                                         binaryOperatorExpr->leftValue->endPosition(),
                                                                         deepCopyAndSimplifyType(rightType),
                                                                         binaryOperatorExpr->leftValue);
                    binaryOperatorExpr->resultType = (deepCopyAndSimplifyType(rightType));
                    return;
                }
            } else { // left is not signed...
                if (rightIsSigned) {
                    binaryOperatorExpr->rightValue = new ImplicitCastExpr(binaryOperatorExpr->rightValue->startPosition(),
                                                                          binaryOperatorExpr->rightValue->endPosition(),
                                                                          deepCopyAndSimplifyType(leftType),
                                                                          binaryOperatorExpr->rightValue);
                    binaryOperatorExpr->resultType = (deepCopyAndSimplifyType(leftType));
                    return;
                }
            }

            // If one side is floating and the other isn't then we cast to the floating side.
            if (leftIsFloating) {
                if (!rightIsFloating) {
                    binaryOperatorExpr->leftValue = new ImplicitCastExpr(binaryOperatorExpr->leftValue->startPosition(),
                                                                         binaryOperatorExpr->leftValue->endPosition(),
                                                                         deepCopyAndSimplifyType(rightType),
                                                                         binaryOperatorExpr->leftValue);
                    binaryOperatorExpr->resultType = (deepCopyAndSimplifyType(rightType));
                    return;
                }
            } else { // left is not signed...
                if (rightIsFloating) {
                    binaryOperatorExpr->rightValue = new ImplicitCastExpr(binaryOperatorExpr->rightValue->startPosition(),
                                                                          binaryOperatorExpr->rightValue->endPosition(),
                                                                          deepCopyAndSimplifyType(leftType),
                                                                          binaryOperatorExpr->rightValue);
                    binaryOperatorExpr->resultType = (deepCopyAndSimplifyType(leftType));
                    return;
                }
            }

            // If one side is bigger than the other then we casting to the bigger side.
            if (leftBuiltInType->size() > rightBuiltInType->size()) {
                binaryOperatorExpr->rightValue = new ImplicitCastExpr(binaryOperatorExpr->rightValue->startPosition(),
                                                                      binaryOperatorExpr->rightValue->endPosition(),
                                                                      deepCopyAndSimplifyType(leftType),
                                                                      binaryOperatorExpr->rightValue);
                binaryOperatorExpr->resultType = (deepCopyAndSimplifyType(leftType));
                return;
            } else if (leftBuiltInType->size() < rightBuiltInType->size()) {
                binaryOperatorExpr->leftValue = new ImplicitCastExpr(binaryOperatorExpr->leftValue->startPosition(),
                                                                     binaryOperatorExpr->leftValue->endPosition(),
                                                                     deepCopyAndSimplifyType(rightType),
                                                                     binaryOperatorExpr->leftValue);
                binaryOperatorExpr->resultType = (deepCopyAndSimplifyType(rightType));
                return;
            }

            // If we reach this point then both are exactly the same type in everything but name. We do nothing if there isn't a custom implicit cast operator for each type name
        }
    }

    binaryOperatorExpr->resultType = deepCopyAndSimplifyType(binaryOperatorExpr->leftValue->resultType);
}

void DeclResolver::processCharacterLiteralExpr(CharacterLiteralExpr *characterLiteralExpr) {
    // TODO: Type suffix support
    characterLiteralExpr->resultType = new BuiltInType({}, {}, "char");
}

void DeclResolver::processExplicitCastExpr(ExplicitCastExpr *explicitCastExpr) {
    explicitCastExpr->resultType = deepCopyAndSimplifyType(explicitCastExpr->castType);
}

void DeclResolver::processFloatLiteralExpr(FloatLiteralExpr *floatLiteralExpr) {
    // TODO: Type suffix support
    floatLiteralExpr->resultType = new BuiltInType({}, {}, "float");
}

void DeclResolver::processFunctionCallExpr(FunctionCallExpr *functionCallExpr) {
    // TODO: Support overloading the operator ()
    for (Expr*& arg : functionCallExpr->arguments) {
        processExpr(arg);

        // NOTE: `convertLValueToRValue` is called AFTER resolving the declaration...
        //convertLValueToRValue(arg);
    }

    if (functionCallExpr->hasArguments()) {
        functionCallArgs = &functionCallExpr->arguments;
    }

    // We process the function reference expression to HOPEFULLY have `functionCallExpr->resultType` be a `FunctionPointerType` if it isn't then we error
    processExpr(functionCallExpr->functionReference);

    // TODO: Support `ConstType`, `MutType`, and `ImmutType` since they can all contain `FunctionPointerType`
    if (llvm::isa<FunctionPointerType>(functionCallExpr->functionReference->resultType)) {
        auto functionPointerType = llvm::dyn_cast<FunctionPointerType>(functionCallExpr->functionReference->resultType);

        for (std::size_t i = 0; i < functionCallExpr->arguments.size(); ++i) {
            // TODO: We will also have to handle implicit casting at some point...
            if (!getTypeIsReference(functionPointerType->paramTypes[i])) {
                convertLValueToRValue(functionCallExpr->arguments[i]);
            }
        }

        // We set the result type of this expression to the result type of the function being called.
        functionCallExpr->resultType = deepCopyAndSimplifyType(functionPointerType->resultType);
        // This makes references technically rvalue references internally...
        functionCallExpr->resultType->setIsLValue(false);
    } else {
        printError("expression is not a valid function reference!",
                   functionCallExpr->functionReference->startPosition(),
                   functionCallExpr->functionReference->endPosition());
    }

    functionCallArgs = nullptr;
}

/**
 * Try to find the `IdentifierExpr` within the current context by searching local variables, params, decls, etc.
 */
void DeclResolver::processIdentifierExpr(Expr*& expr) {
    auto identifierExpr = llvm::dyn_cast<IdentifierExpr>(expr);

    // First we check if the identifier is a built in type
    if (BuiltInType::isBuiltInType(identifierExpr->name())) {
        // TODO: Support template overloading. Allow someone to implement `struct int<T> {}` that will be found if there are template arguments
        if (identifierExpr->hasTemplateArguments()) {
            printError("built in types do not support templating!",
                       identifierExpr->startPosition(), identifierExpr->endPosition());
        }

        Type *resolvedType = new BuiltInType(expr->startPosition(), expr->endPosition(), identifierExpr->name());

        delete identifierExpr;

        expr = new ResolvedTypeRefExpr(expr->startPosition(), expr->endPosition(), resolvedType);
        // TODO: Is this really even needed?
        expr->resultType = deepCopyAndSimplifyType(resolvedType);
        return;
    }

    if (identifierExpr->hasTemplateArguments()) {
        printError("templating support currently not finished!",
                   identifierExpr->startPosition(), identifierExpr->endPosition());
        return;
    }

    // Then we check local variables
    for (std::size_t i = 0; i < functionLocalVariablesCount; ++i) {
        if (functionLocalVariables[i]->name() == identifierExpr->name()) {
            if (identifierExpr->hasTemplateArguments()) {
                printError("local variable references cannot have template arguments!",
                           identifierExpr->startPosition(), identifierExpr->endPosition());
            }

            expr = new RefLocalVariableExpr(identifierExpr->startPosition(), identifierExpr->endPosition(),
                                            identifierExpr->name());
            expr->resultType = deepCopyAndSimplifyType(functionLocalVariables[i]->resultType);
            // A local variable reference is an lvalue.
            expr->resultType->setIsLValue(true);
            // We always dereference local variable calls if they are references...
            dereferenceReferences(expr);

            delete identifierExpr;

            return;
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

                expr = new RefParameterExpr(identifierExpr->startPosition(), identifierExpr->endPosition(),
                                            paramIndex);
                expr->resultType = deepCopyAndSimplifyType((*functionParams)[paramIndex]->type);
                // A parameter reference is an lvalue.
                expr->resultType->setIsLValue(true);
                // We always dereference local variable calls if they are references...
                dereferenceReferences(expr);

                delete identifierExpr;

                return;
            }
        }
    }

    // then function template params
    if (functionTemplateParams != nullptr) {
        for (const TemplateParameterDecl* templateParam : *functionTemplateParams) {
            // TODO: Check if the type of `templateParam` is `FunctionPointerType`
            if (templateParam->name() == identifierExpr->name()) {
                identifierExpr->resultType = deepCopyAndSimplifyType(templateParam->type);
                return;
            }
        }
    }

    // TODO: then class/struct members
    // TODO: then class/struct template params

    // then current file
    const FunctionDecl* foundFunction = nullptr;

    for (const Decl* decl : currentFileAst->topLevelDecls()) {
        if (decl->name() == identifierExpr->name()) {
            // This is currently the only `Decl` we support at the top level
            if (llvm::isa<FunctionDecl>(decl)) {
                // TODO: We need to set something in the context to allow us to know what the function calls current parameters are.
                auto functionDecl = llvm::dyn_cast<FunctionDecl>(decl);

                if (functionDecl->name() == identifierExpr->name()) {
                    if (!functionDecl->hasParameters()) {
                        // If the function doesn't have parameters and there aren't any arguments then we've found a perfect match...
                        if (functionCallArgs == nullptr || functionCallArgs->empty()) {
                            // It is a perfect match, set the foundFunction variable so we can create the `FunctionPointerType`
                            foundFunction = functionDecl;
                            goto functionWasFound; // This is why we support breaking from labeled loops...
                        } else {
                            // Args don't match params at all, keep looking.
                            continue;
                        }
                    } else { // Else the function decl has parameters...
                        // If the function call doesn't have args then it doesn't match...
                        if (functionCallArgs == nullptr || functionCallArgs->empty()) {
                            // Keep looking...
                            continue;
                        }

                        // Else we compare the params and arguments
                        // NOTE: There can be fewer arguments than there are parameters due to optional parameters
                        // because of this we keep the current index outside of the loop so that if it is still less
                        // than the length of the `FunctionDecl::parameters` we can make sure the rest of the parameters
                        // are optional.
                        std::size_t i = 0;
                        bool argsMatch = true;

                        for (; i < functionCallArgs->size(); ++i) {
                            if (i >= functionDecl->parameters.size()) {
                                argsMatch = false;
                                break;
                            }

                            Type* argType = (*(functionCallArgs))[i]->resultType;
                            Type* paramType = functionDecl->parameters[i]->type;

                            if (llvm::isa<ReferenceType>(argType)) argType = llvm::dyn_cast<ReferenceType>(argType)->referenceToType;
                            if (llvm::isa<RValueReferenceType>(argType)) argType = llvm::dyn_cast<RValueReferenceType>(argType)->referenceToType;
                            if (llvm::isa<ReferenceType>(paramType)) paramType = llvm::dyn_cast<ReferenceType>(paramType)->referenceToType;
                            if (llvm::isa<RValueReferenceType>(paramType)) paramType = llvm::dyn_cast<RValueReferenceType>(paramType)->referenceToType;

                            // TODO: Support checking if something can be implicitly casted properly.
                            if (!getTypesAreSame(argType, paramType)) {
                                argsMatch = false;
                                break;
                            }
                        }

                        if (argsMatch) {
                            // If the args match then we check for the last few parameters to make sure they're all
                            // optional (assuming there are remaining parameters)
                            for (; i < functionDecl->parameters.size(); ++i) {
                                if (functionDecl->parameters[i]->hasDefaultArgument()) {
                                    // If the first index we check has an optional parameter then the rest are all optional, it would be useless to check the rest.
                                    argsMatch = true;
                                    break;
                                } else {
                                    // If the first index we check is not optional then it doesn't matter what the rest are, we're missing a required argument.
                                    argsMatch = false;
                                    break;
                                }
                            }

                            if (argsMatch) {
                                // TODO: We need to support checking for ambiguities
                                foundFunction = functionDecl;
                                goto functionWasFound; // This is why we support breaking from labeled loops...
                            }
                        }
                    }
                }
            }
        }
    }

functionWasFound:

    if (foundFunction) {
        Type* resultTypeCopy = deepCopyAndSimplifyType(foundFunction->resultType);
        std::vector<Type*> paramTypeCopy{};

        for (const ParameterDecl* param : foundFunction->parameters) {
            paramTypeCopy.push_back(deepCopyAndSimplifyType(param->type));
        }

        // TODO: the tempalte arguments need to be processed
        expr = new RefFileFunctionExpr(identifierExpr->startPosition(), identifierExpr->endPosition(),
                                       identifierExpr->name(), identifierExpr->templateArguments);
        expr->resultType = new FunctionPointerType({}, {}, resultTypeCopy, paramTypeCopy);

        delete identifierExpr;

        return;
    }

    // TODO: then current namespace
    // TODO: then imports.

    // if we've made it to this point the identifier cannot be found, error and tell the user.
    printError("identifier '" + identifierExpr->name() + "' was not found in the current context!",
               identifierExpr->startPosition(), identifierExpr->endPosition());
}

void DeclResolver::processImplicitCastExpr(ImplicitCastExpr *implicitCastExpr) {
    if (implicitCastExpr->resultType == nullptr) {
        implicitCastExpr->resultType = (deepCopyAndSimplifyType(implicitCastExpr->castType));
    }
}

void DeclResolver::processIndexerCallExpr(IndexerCallExpr *indexerCallExpr) {
    // TODO: Support overloading the indexer operator []
    processExpr(indexerCallExpr->indexerReference);

    for (Expr*& argument : indexerCallExpr->arguments()) {
        processExpr(argument);
    }
    printError("array indexer calls not yet supported!", indexerCallExpr->startPosition(), indexerCallExpr->endPosition());
}

void DeclResolver::processIntegerLiteralExpr(IntegerLiteralExpr *integerLiteralExpr) {
    // TODO: Type suffix support
    integerLiteralExpr->resultType = (new BuiltInType({}, {}, "int"));
}

void DeclResolver::processLocalVariableDeclExpr(LocalVariableDeclExpr *localVariableDeclExpr) {
    // We process the type as an Expr so that we can resolve any potentially unresolved types...
    processExpr(localVariableDeclExpr->type);

    if (llvm::isa<ResolvedTypeRefExpr>(localVariableDeclExpr->type)) {
        if (localVariableNameTaken(localVariableDeclExpr->name())) {
            printError("redefinition of variable '" + localVariableDeclExpr->name() + "' not allowed!",
                       localVariableDeclExpr->startPosition(),
                       localVariableDeclExpr->endPosition());
        }

        auto resolvedTypeRefExpr = llvm::dyn_cast<ResolvedTypeRefExpr>(localVariableDeclExpr->type);
        localVariableDeclExpr->resultType = deepCopyAndSimplifyType(resolvedTypeRefExpr->resolvedType);

        addLocalVariable(localVariableDeclExpr);
    }
}

void DeclResolver::processLocalVariableDeclOrPrefixOperatorCallExpr(Expr *&expr) {
    auto localVariableDeclOrPrefixOperatorCallExpr = llvm::dyn_cast<LocalVariableDeclOrPrefixOperatorCallExpr>(expr);

    // TODO: Currently we only support `Identifier` types.
    if (localVariableDeclOrPrefixOperatorCallExpr->nameOrExpr->getExprKind() == Expr::Kind::Identifier) {
        processExpr(localVariableDeclOrPrefixOperatorCallExpr->typeOrPrefixOperator);

        // If the type of 'typeOrPrefixOperator is now 'ResolvedTypeRef' then that means this is a local variable. If not then it is a prefix operator OR the type just isn't in scope.
        if (localVariableDeclOrPrefixOperatorCallExpr->typeOrPrefixOperator->getExprKind() == Expr::Kind::ResolvedTypeRef) {
            Expr* type = localVariableDeclOrPrefixOperatorCallExpr->typeOrPrefixOperator;

            if (localVariableDeclOrPrefixOperatorCallExpr->nameOrExpr->getExprKind() != Expr::Kind::Identifier) {
                printError("unknown expression in local variable declaration, expected variable name!",
                           localVariableDeclOrPrefixOperatorCallExpr->nameOrExpr->startPosition(),
                           localVariableDeclOrPrefixOperatorCallExpr->nameOrExpr->endPosition());
                return;
            }

            auto identifier = llvm::dyn_cast<IdentifierExpr>(localVariableDeclOrPrefixOperatorCallExpr->nameOrExpr);

            // Local variable names cannot have template arguments in the name
            if (identifier->hasTemplateArguments()) {
                printError("unknown expression in local variable declaration, expected variable name!",
                           localVariableDeclOrPrefixOperatorCallExpr->nameOrExpr->startPosition(),
                           localVariableDeclOrPrefixOperatorCallExpr->nameOrExpr->endPosition());
                return;
            }

            std::string variableName = identifier->name();

            expr = new LocalVariableDeclExpr(localVariableDeclOrPrefixOperatorCallExpr->startPosition(),
                                             localVariableDeclOrPrefixOperatorCallExpr->endPosition(),
                                             type, variableName);

            processLocalVariableDeclExpr(llvm::dyn_cast<LocalVariableDeclExpr>(expr));

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

        processExpr(argument);

        PrefixOperatorExpr* prefixOperatorExpr = new PrefixOperatorExpr(localVariableDeclOrPrefixOperatorCallExpr->startPosition(),
                                                                        localVariableDeclOrPrefixOperatorCallExpr->endPosition(),
                                                                        operatorName, argument);
        processPrefixOperatorExpr(prefixOperatorExpr);
        expr = prefixOperatorExpr;

        delete localVariableDeclOrPrefixOperatorCallExpr;
        return;
    } else {
        printError("unknown expression where local variable declaration or prefix operator call was expected!",
                   localVariableDeclOrPrefixOperatorCallExpr->typeOrPrefixOperator->startPosition(),
                   localVariableDeclOrPrefixOperatorCallExpr->typeOrPrefixOperator->endPosition());
        return;
    }
}

void DeclResolver::processMemberAccessCallExpr(MemberAccessCallExpr *memberAccessCallExpr) {
    // TODO: Support operator overloading the `.` and `->` operators
    // TODO: `public override T operator.() => return this.whatever_T_is;` this can ONLY be supported when the implementing class/struct has NO public facing functions
    // TODO: MemberAccessCallExpr can ALSO be a namespace path to a type. We will need to take this into account at some point.
//    processExpr(memberAccessCallExpr->objectRef);
//    processIdentifierExpr(memberAccessCallExpr->member);
    printError("member access calls not yet supported!", memberAccessCallExpr->startPosition(), memberAccessCallExpr->endPosition());
}

void DeclResolver::processParenExpr(ParenExpr *parenExpr) {
    processExpr(parenExpr->containedExpr);

    parenExpr->resultType = deepCopyAndSimplifyType(parenExpr->containedExpr->resultType);

    convertLValueToRValue(parenExpr->containedExpr);
}

void DeclResolver::processPostfixOperatorExpr(PostfixOperatorExpr *postfixOperatorExpr) {
    // TODO: Support operator overloading type resolution
    processExpr(postfixOperatorExpr->expr);

    postfixOperatorExpr->resultType = deepCopyAndSimplifyType(postfixOperatorExpr->expr->resultType);
}

void DeclResolver::processPotentialExplicitCastExpr(Expr*& expr) {
    auto potentialExplicitCastExpr = llvm::dyn_cast<PotentialExplicitCastExpr>(expr);

    processExpr(potentialExplicitCastExpr->castType);
    processExpr(potentialExplicitCastExpr->castee);

    if (!llvm::isa<ResolvedTypeRefExpr>(potentialExplicitCastExpr->castType)) {
        printError("unknown type in explicit cast expression!",
                   potentialExplicitCastExpr->startPosition(), potentialExplicitCastExpr->endPosition());
    }

    auto resolvedTypeRef = llvm::dyn_cast<ResolvedTypeRefExpr>(potentialExplicitCastExpr->castType);
    Type* resolvedType = resolvedTypeRef->resolvedType;

    expr = new ExplicitCastExpr(potentialExplicitCastExpr->startPosition(), potentialExplicitCastExpr->endPosition(),
                                resolvedType, potentialExplicitCastExpr->castee);

    expr->resultType = deepCopyAndSimplifyType(resolvedType);

    // Set the values we steal to nullptr then delete the old expression
    resolvedTypeRef->resolvedType = nullptr;
    potentialExplicitCastExpr->castee = nullptr;
    delete potentialExplicitCastExpr;
}

void DeclResolver::processPrefixOperatorExpr(PrefixOperatorExpr *prefixOperatorExpr) {
    processExpr(prefixOperatorExpr->expr);

    gulc::Type* checkType = prefixOperatorExpr->expr->resultType;

    if (llvm::isa<MutType>(checkType)) {
        checkType = llvm::dyn_cast<MutType>(checkType)->pointToType;
    } else if (llvm::isa<ConstType>(checkType)) {
        checkType = llvm::dyn_cast<ConstType>(checkType)->pointToType;
    } else if (llvm::isa<ImmutType>(checkType)) {
        checkType = llvm::dyn_cast<ImmutType>(checkType)->pointToType;
    }

    // TODO: Support operator overloading type resolution
    // TODO: Support `sizeof`, `alignof`, `offsetof`, and `nameof`
    if (prefixOperatorExpr->operatorName() == "&") { // Address
        prefixOperatorExpr->resultType = new PointerType({}, {}, deepCopyAndSimplifyType(prefixOperatorExpr->expr->resultType));
    } else if (prefixOperatorExpr->operatorName() == "*") { // Dereference
        if (llvm::isa<PointerType>(checkType)) {
            auto pointerType = llvm::dyn_cast<PointerType>(checkType);
            prefixOperatorExpr->resultType = deepCopyAndSimplifyType(pointerType->pointToType);
            // A dereferenced value is an lvalue
            prefixOperatorExpr->resultType->setIsLValue(true);
        } else {
            printError("cannot dereference non-pointer type `" + prefixOperatorExpr->expr->resultType->getString() + "`!",
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

        prefixOperatorExpr->resultType = new ReferenceType({}, {}, deepCopyAndSimplifyType(prefixOperatorExpr->expr->resultType));
    } else if (prefixOperatorExpr->operatorName() == ".deref") {
        if (llvm::isa<ReferenceType>(checkType)) {
            auto referenceType = llvm::dyn_cast<ReferenceType>(checkType);
            prefixOperatorExpr->resultType = deepCopyAndSimplifyType(referenceType->referenceToType);
            // A dereferenced value is an lvalue
            prefixOperatorExpr->resultType->setIsLValue(true);
        } else {
            printError("[INTERNAL] expected reference type!", prefixOperatorExpr->expr->startPosition(), prefixOperatorExpr->expr->endPosition());
            return;
        }
    } else {
        prefixOperatorExpr->resultType = deepCopyAndSimplifyType(prefixOperatorExpr->expr->resultType);
    }
}

void DeclResolver::processResolvedTypeRefExpr(ResolvedTypeRefExpr *resolvedTypeRefExpr) {

}

void DeclResolver::processStringLiteralExpr(StringLiteralExpr *stringLiteralExpr) {
    // TODO: Type suffix support
    stringLiteralExpr->resultType = (new PointerType({}, {}, new BuiltInType({}, {}, "char")));
}

void DeclResolver::processTernaryExpr(TernaryExpr *ternaryExpr) {
    processExpr(ternaryExpr->condition);
    // TODO: Verify both expressions are the same type
    processExpr(ternaryExpr->trueExpr);
    processExpr(ternaryExpr->falseExpr);
}

void DeclResolver::processUnresolvedTypeRefExpr(Expr *&expr) {
    auto unresolvedTypeRefExpr = llvm::dyn_cast<UnresolvedTypeRefExpr>(expr);
    Type* resolvedType = unresolvedTypeRefExpr->unresolvedType;

    if (!resolveType(resolvedType)) {
        // TODO: Better error message
        printError("could not resolve type!",
                   resolvedType->startPosition(), resolvedType->endPosition());
    }

    unresolvedTypeRefExpr->unresolvedType = nullptr;

    expr = new ResolvedTypeRefExpr(unresolvedTypeRefExpr->startPosition(), unresolvedTypeRefExpr->endPosition(), resolvedType);
    delete unresolvedTypeRefExpr;
}

void DeclResolver::dereferenceReferences(Expr*& potentialReference) {
    Type* checkType = potentialReference->resultType;

    if (llvm::isa<MutType>(checkType)) {
        checkType = llvm::dyn_cast<MutType>(checkType)->pointToType;
    } else if (llvm::isa<ConstType>(checkType)) {
        checkType = llvm::dyn_cast<ConstType>(checkType)->pointToType;
    } else if (llvm::isa<ImmutType>(checkType)) {
        checkType = llvm::dyn_cast<ImmutType>(checkType)->pointToType;
    }

    // For references we convert the reference using the prefix operator `.deref`
    if (llvm::isa<ReferenceType>(checkType)) {
        auto referenceType = llvm::dyn_cast<ReferenceType>(checkType);

        if (referenceType->isLValue()) {
            auto newPotentialReference = new PrefixOperatorExpr(potentialReference->startPosition(),
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
        newRValue->resultType = deepCopyAndSimplifyType(potentialLValue->resultType);
        newRValue->resultType->setIsLValue(false);
        potentialLValue = newRValue;
    }
}
