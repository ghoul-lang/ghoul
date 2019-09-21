#include <AST/Types/BuiltInType.hpp>
#include <AST/Types/PointerType.hpp>
#include <AST/Types/FunctionTemplateTypenameRefType.hpp>
#include <AST/Types/TemplateTypenameType.hpp>
#include <AST/Types/UnresolvedType.hpp>
#include <AST/Types/FunctionPointerType.hpp>
#include <AST/Exprs/LValueToRValueExpr.hpp>
#include <AST/Exprs/UnresolvedTypeRefExpr.hpp>
#include <AST/Exprs/LocalVariableDeclOrPrefixOperatorCallExpr.hpp>
#include "DeclResolver.hpp"

using namespace gulc;

void DeclResolver::processFile(FileAST &fileAst) {
    currentFileAst = &fileAst;

    for (Decl* decl : fileAst.topLevelDecls()) {
        processDecl(fileAst, decl);
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
    }

    return false;
}

// TODO: Should we just combine with `getTypeGreaterThan` as a `compareTypes`?
bool DeclResolver::getTypesAreSame(const Type* type1, const Type* type2) {
    if (type1->getTypeKind() == type2->getTypeKind()) {
        switch (type1->getTypeKind()) {
            case Type::Kind::BuiltIn: {
                auto builtInType1 = llvm::dyn_cast<BuiltInType>(type1);
                auto builtInType2 = llvm::dyn_cast<BuiltInType>(type2);

                return builtInType1->name() == builtInType2->name();
            }
            case Type::Kind::FunctionTemplateTypenameRef: {
                auto pointerType1 = llvm::dyn_cast<PointerType>(type1);
                auto pointerType2 = llvm::dyn_cast<PointerType>(type2);

                return getTypesAreSame(pointerType1->pointToType, pointerType2->pointToType);
            }
            case Type::Kind::Pointer: {
                auto pointerType1 = llvm::dyn_cast<PointerType>(type1);
                auto pointerType2 = llvm::dyn_cast<PointerType>(type2);

                return getTypesAreSame(pointerType1->pointToType, pointerType2->pointToType);
            }
			case Type::Kind::FunctionPointer: {
				auto funcPointerType1 = llvm::dyn_cast<FunctionPointerType>(type1);
				auto funcPointerType2 = llvm::dyn_cast<FunctionPointerType>(type2);

				if (!getTypesAreSame(funcPointerType1->resultType, funcPointerType2->resultType)) {
					return false;
				}

				if (funcPointerType1->paramTypes.empty() != funcPointerType2->paramTypes.empty()) {
					return false;
				}

				if (!funcPointerType1->paramTypes.empty()) {
					for (std::size_t i = 0; i < funcPointerType1->paramTypes.size(); ++i) {
						if (!getTypesAreSame(funcPointerType1->paramTypes[i], funcPointerType2->paramTypes[i])) {
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
    }

    return false;
}

bool DeclResolver::getTypeGreaterThan(const Type* left, const Type* right) {
    return false;
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

void DeclResolver::printDebugWarning(const std::string &message) {
#ifndef NDEBUG
    std::cout << "gulc qualify type pass [DEBUG WARNING](" << currentFileAst->filePath() << "): " << message << std::endl;
#endif
}

void DeclResolver::processDecl(FileAST &fileAst, Decl *decl) {
    switch (decl->getDeclKind()) {
        case Decl::Kind::Function:
            processFunctionDecl(fileAst, llvm::dyn_cast<FunctionDecl>(decl));
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
            processBinaryOperatorExpr(llvm::dyn_cast<BinaryOperatorExpr>(expr));
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
            processPotentialExplicitCastExpr(llvm::dyn_cast<PotentialExplicitCastExpr>(expr));
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
void DeclResolver::processFunctionDecl(FileAST &fileAst, FunctionDecl *functionDecl) {
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

        convertLValueToRValue(returnStmt->returnValue);

        if (!getTypesAreSame(returnStmt->returnValue->resultType, returnType)) {
            // TODO: Check if `returnValueType` can be implicitly casted to the `returnType`
            printError("return value type does not match the function return type!",
                       returnStmt->returnValue->startPosition(), returnStmt->returnValue->endPosition());
            return;
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
void DeclResolver::processBinaryOperatorExpr(BinaryOperatorExpr *binaryOperatorExpr) {
    // TODO: Support operator overloading type resolution
    processExpr(binaryOperatorExpr->leftValue);
    processExpr(binaryOperatorExpr->rightValue);

    convertLValueToRValue(binaryOperatorExpr->rightValue);

    Type* leftType = binaryOperatorExpr->leftValue->resultType;
    Type* rightType = binaryOperatorExpr->rightValue->resultType;

    if (binaryOperatorExpr->isBuiltInAssignmentOperator()) {
        if (!getTypesAreSame(leftType, rightType)) {
            binaryOperatorExpr->rightValue = new ImplicitCastExpr(binaryOperatorExpr->rightValue->startPosition(),
                                                                  binaryOperatorExpr->rightValue->endPosition(),
                                                                  deepCopyAndSimplifyType(leftType),
                                                                  binaryOperatorExpr->rightValue);
        }

        binaryOperatorExpr->resultType = deepCopyAndSimplifyType(leftType);

        if (binaryOperatorExpr->operatorName() != "=") {
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

            // We set a new right value with an `LValueToRValueExpr` that doesn't own the pointer. Making it so the left L2R expression doesn't delete the pointer since the binary operator owns it.
            auto newRightValue = new BinaryOperatorExpr(binaryOperatorExpr->startPosition(),
                                                        binaryOperatorExpr->endPosition(),
                                                        rightOperatorName,
                                                        new LValueToRValueExpr({}, {}, binaryOperatorExpr->leftValue, false),
                                                        binaryOperatorExpr->rightValue);

            newRightValue->resultType = deepCopyAndSimplifyType(leftType);

            // Set the new right value and change the operator name to '='
            binaryOperatorExpr->setOperatorName("=");
            binaryOperatorExpr->rightValue = newRightValue;
        }

        return;
    } else {
        convertLValueToRValue(binaryOperatorExpr->leftValue);

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

        convertLValueToRValue(arg);
    }

    if (functionCallExpr->hasArguments()) {
        functionCallArgs = &functionCallExpr->arguments;
    }

    // We process the function reference expression to HOPEFULLY have `functionCallExpr->resultType` be a `FunctionPointerType` if it isn't then we error
    processExpr(functionCallExpr->functionReference);

    // TODO: Support `ConstType`, `MutType`, and `ImmutType` since they can all contain `FunctionPointerType`
    if (llvm::isa<FunctionPointerType>(functionCallExpr->functionReference->resultType)) {
        auto functionPointerType = llvm::dyn_cast<FunctionPointerType>(functionCallExpr->functionReference->resultType);

        // We set the result type of this expression to the result type of the function being called.
        functionCallExpr->resultType = deepCopyAndSimplifyType(functionPointerType->resultType);
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
            identifierExpr->resultType = deepCopyAndSimplifyType(functionLocalVariables[i]->resultType);
            return;
        }
    }

    // then params
    if (functionParams != nullptr) {
        for (const ParameterDecl* param : *functionParams) {
            if (param->name() == identifierExpr->name()) {
                identifierExpr->resultType = deepCopyAndSimplifyType(param->type);
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

                            // TODO: Support checking if something can be implicitly casted properly.
                            if (!getTypesAreSame((*(functionCallArgs))[i]->resultType, functionDecl->resultType)) {
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

        identifierExpr->resultType = new FunctionPointerType({}, {}, resultTypeCopy, paramTypeCopy);
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
    if (llvm::isa<ResolvedTypeRefExpr>(localVariableDeclExpr->type())) {
        if (localVariableNameTaken(localVariableDeclExpr->name())) {
            printError("redefinition of variable '" + localVariableDeclExpr->name() + "' not allowed!",
                       localVariableDeclExpr->startPosition(),
                       localVariableDeclExpr->endPosition());
        }

        auto resolvedTypeRefExpr = llvm::dyn_cast<ResolvedTypeRefExpr>(localVariableDeclExpr->type());
        localVariableDeclExpr->resultType = deepCopyAndSimplifyType(resolvedTypeRefExpr->resolvedType());

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

        expr = new PrefixOperatorExpr(localVariableDeclOrPrefixOperatorCallExpr->startPosition(),
                                      localVariableDeclOrPrefixOperatorCallExpr->endPosition(),
                                      operatorName, argument);

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

void DeclResolver::processPotentialExplicitCastExpr(PotentialExplicitCastExpr *potentialExplicitCastExpr) {
    processExpr(potentialExplicitCastExpr->castType);
    processExpr(potentialExplicitCastExpr->castee);
}

void DeclResolver::processPrefixOperatorExpr(PrefixOperatorExpr *prefixOperatorExpr) {
    processExpr(prefixOperatorExpr->expr);

    // TODO: Support operator overloading type resolution
    if (prefixOperatorExpr->operatorName() == "&") { // Address
        prefixOperatorExpr->resultType = (new PointerType({}, {}, deepCopyAndSimplifyType(prefixOperatorExpr->expr->resultType)));
    } else if (prefixOperatorExpr->operatorName() == "*") { // Dereference
        if (llvm::isa<PointerType>(prefixOperatorExpr->expr->resultType)) {
            auto pointerType = llvm::dyn_cast<PointerType>(prefixOperatorExpr->expr->resultType);
            Type* dereferencedType = pointerType->pointToType;
            pointerType->pointToType = nullptr;
            delete pointerType;
            prefixOperatorExpr->resultType = dereferencedType;
        } else {
            printError("cannot dereference non-pointer type `" + prefixOperatorExpr->expr->resultType->getString() + "`!",
                       prefixOperatorExpr->startPosition(), prefixOperatorExpr->endPosition());
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

void DeclResolver::convertLValueToRValue(Expr*& potentialLValue) {
    // TODO: IdentifierExpr isn't the only thing we will have to do this for, right?
    if (llvm::isa<IdentifierExpr>(potentialLValue)) {
        Expr* newRValue = new LValueToRValueExpr(potentialLValue->startPosition(), potentialLValue->endPosition(), potentialLValue);
        newRValue->resultType = deepCopyAndSimplifyType(potentialLValue->resultType);
        potentialLValue = newRValue;
    }
}
