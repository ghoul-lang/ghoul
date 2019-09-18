#include <AST/Types/BuiltInType.hpp>
#include <AST/Types/PointerType.hpp>
#include <AST/Types/FunctionTemplateTypenameRefType.hpp>
#include <AST/Types/TemplateTypenameType.hpp>
#include <AST/Types/UnresolvedType.hpp>
#include <AST/Types/FunctionPointerType.hpp>
#include <AST/Exprs/LValueToRValueExpr.hpp>
#include "DeclResolverPass.hpp"

using namespace gulc;

void DeclResolverPass::processFile(FileAST &fileAst) {
    for (Decl* decl : fileAst.topLevelDecls()) {
        processDecl(fileAst, decl);
    }
}

// TODO: Should we just combine with `getTypeGreaterThan` as a `compareTypes`?
bool DeclResolverPass::getTypesAreSame(const Type* type1, const Type* type2) {
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

bool DeclResolverPass::getTypeGreaterThan(const Type* left, const Type* right) {
    return false;
}

Type *DeclResolverPass::deepCopyAndSimplifyType(const Type *type) {
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

void DeclResolverPass::printError(const std::string &message, FileAST &fileAst, TextPosition startPosition, TextPosition endPosition) {
    std::cout << "gulc resolver error[" << fileAst.filePath() << ", "
                                     "{" << startPosition.line << ", " << startPosition.column << "} "
                                     "to {" << endPosition.line << ", " << endPosition.column << "}]: "
              << message
              << std::endl;
    std::exit(1);
}

void DeclResolverPass::printDebugWarning(const std::string &message, FileAST &fileAst) {
#ifndef NDEBUG
    std::cout << "gulc qualify type pass [DEBUG WARNING](" << fileAst.filePath() << "): " << message << std::endl;
#endif
}

void DeclResolverPass::processDecl(FileAST &fileAst, Decl *decl) {
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

void DeclResolverPass::processStmt(ResolveDeclsContext &context, Stmt *&stmt) {
    switch (stmt->getStmtKind()) {
        case Stmt::Kind::Break:
            processBreakStmt(context, llvm::dyn_cast<BreakStmt>(stmt));
            break;
        case Stmt::Kind::Case:
            processCaseStmt(context, llvm::dyn_cast<CaseStmt>(stmt));
            break;
        case Stmt::Kind::Compound:
            processCompoundStmt(context, llvm::dyn_cast<CompoundStmt>(stmt));
            break;
        case Stmt::Kind::Continue:
            processContinueStmt(context, llvm::dyn_cast<ContinueStmt>(stmt));
            break;
        case Stmt::Kind::Do:
            processDoStmt(context, llvm::dyn_cast<DoStmt>(stmt));
            break;
        case Stmt::Kind::For:
            processForStmt(context, llvm::dyn_cast<ForStmt>(stmt));
            break;
        case Stmt::Kind::Goto:
            processGotoStmt(context, llvm::dyn_cast<GotoStmt>(stmt));
            break;
        case Stmt::Kind::If:
            processIfStmt(context, llvm::dyn_cast<IfStmt>(stmt));
            break;
        case Stmt::Kind::Labeled:
            processLabeledStmt(context, llvm::dyn_cast<LabeledStmt>(stmt));
            break;
        case Stmt::Kind::Return:
            processReturnStmt(context, llvm::dyn_cast<ReturnStmt>(stmt));
            break;
        case Stmt::Kind::Switch:
            processSwitchStmt(context, llvm::dyn_cast<SwitchStmt>(stmt));
            break;
        case Stmt::Kind::Try:
            processTryStmt(context, llvm::dyn_cast<TryStmt>(stmt));
            break;
        case Stmt::Kind::TryCatch:
            processTryCatchStmt(context, llvm::dyn_cast<TryCatchStmt>(stmt));
            break;
        case Stmt::Kind::TryFinally:
            processTryFinallyStmt(context, llvm::dyn_cast<TryFinallyStmt>(stmt));
            break;
        case Stmt::Kind::While:
            processWhileStmt(context, llvm::dyn_cast<WhileStmt>(stmt));
            break;
        case Stmt::Kind::Expr: {
            auto expr = llvm::dyn_cast<Expr>(stmt);
            processExpr(context, expr);
            stmt = expr;
        }
    }
}

void DeclResolverPass::processExpr(ResolveDeclsContext &context, Expr *&expr) {
    switch (expr->getExprKind()) {
        case Expr::Kind::BinaryOperator:
            processBinaryOperatorExpr(context, llvm::dyn_cast<BinaryOperatorExpr>(expr));
            break;
        case Expr::Kind::CharacterLiteral:
            processCharacterLiteralExpr(context, llvm::dyn_cast<CharacterLiteralExpr>(expr));
            break;
        case Expr::Kind::ExplicitCast:
            processExplicitCastExpr(context, llvm::dyn_cast<ExplicitCastExpr>(expr));
            break;
        case Expr::Kind::FloatLiteral:
            processFloatLiteralExpr(context, llvm::dyn_cast<FloatLiteralExpr>(expr));
            break;
        case Expr::Kind::FunctionCall:
            processFunctionCallExpr(context, llvm::dyn_cast<FunctionCallExpr>(expr));
            break;
        case Expr::Kind::Identifier:
            processIdentifierExpr(context, llvm::dyn_cast<IdentifierExpr>(expr));
            break;
        case Expr::Kind::ImplicitCast:
            processImplicitCastExpr(context, llvm::dyn_cast<ImplicitCastExpr>(expr));
            break;
        case Expr::Kind::IndexerCall:
            processIndexerCallExpr(context, llvm::dyn_cast<IndexerCallExpr>(expr));
            break;
        case Expr::Kind::IntegerLiteral:
            processIntegerLiteralExpr(context, llvm::dyn_cast<IntegerLiteralExpr>(expr));
            break;
        case Expr::Kind::LocalVariableDecl:
            processLocalVariableDeclExpr(context, llvm::dyn_cast<LocalVariableDeclExpr>(expr));
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
        case Expr::Kind::ResolvedTypeRef:
            processResolvedTypeRefExpr(context, llvm::dyn_cast<ResolvedTypeRefExpr>(expr));
            break;
        case Expr::Kind::StringLiteral:
            processStringLiteralExpr(context, llvm::dyn_cast<StringLiteralExpr>(expr));
            break;
        case Expr::Kind::Ternary:
            processTernaryExpr(context, llvm::dyn_cast<TernaryExpr>(expr));
            break;
        case Expr::Kind::UnresolvedTypeRef:
            processUnresolvedTypeRefExpr(context, expr);
            break;
    }
}

// Decls
void DeclResolverPass::processFunctionDecl(FileAST &fileAst, FunctionDecl *functionDecl) {
    ResolveDeclsContext context(fileAst);

    if (functionDecl->hasTemplateParameters()) {
        bool shouldHaveDefaultArgument = false;

        // Make sure all template parameters after the first optional template parameter are also optional
        for (const TemplateParameterDecl* templateParameterDecl : functionDecl->templateParameters) {
            if (templateParameterDecl->hasDefaultArgument()) {
                if (!shouldHaveDefaultArgument) {
                    shouldHaveDefaultArgument = true;
                } else {
                    printError("all template parameters after the first optional template parameter must also be optional!",
                               context.fileAst, templateParameterDecl->startPosition(), templateParameterDecl->endPosition());
                }
            }
        }

        context.functionTemplateParams = &functionDecl->templateParameters;
    }

    if (functionDecl->hasParameters()) {
        bool shouldHaveDefaultArgument = false;

        // Make sure all parameters after the first optional parameter are also optional
        for (const ParameterDecl* parameterDecl : functionDecl->parameters) {
            if (parameterDecl->hasDefaultArgument()) {
                if (!shouldHaveDefaultArgument) {
                    shouldHaveDefaultArgument = true;
                } else {
                    printError("all parameters after the first optional parameter must also be optional!",
                               context.fileAst, parameterDecl->startPosition(), parameterDecl->endPosition());
                }
            }
        }

        context.functionParams = &functionDecl->parameters;
    }

    context.returnType = functionDecl->resultType;

    // We reset to zero just in case.
    context.functionLocalVariablesCount = 0;
    processCompoundStmt(context, functionDecl->body());
    context.functionLocalVariablesCount = 0;

    context.returnType = nullptr;
    context.functionParams = nullptr;
    context.functionTemplateParams = nullptr;
}

// Stmts
void DeclResolverPass::processBreakStmt(ResolveDeclsContext &context, BreakStmt *breakStmt) {
    if (!breakStmt->label().empty()) {
        context.addUnresolvedLabel(breakStmt->label());
    }
}

void DeclResolverPass::processCaseStmt(ResolveDeclsContext &context, CaseStmt *caseStmt) {
    if (caseStmt->hasCondition()) {
        processExpr(context, caseStmt->condition);
    }

    processStmt(context, caseStmt->trueStmt);
}

void DeclResolverPass::processCompoundStmt(ResolveDeclsContext &context, CompoundStmt *compoundStmt) {
    unsigned int oldLocalVariableCount = context.functionLocalVariablesCount;

    for (Stmt*& stmt : compoundStmt->statements()) {
        processStmt(context, stmt);
    }

    context.functionLocalVariablesCount = oldLocalVariableCount;
}

void DeclResolverPass::processContinueStmt(ResolveDeclsContext &context, ContinueStmt *continueStmt) {
    if (!continueStmt->label().empty()) {
        context.addUnresolvedLabel(continueStmt->label());
    }
}

void DeclResolverPass::processDoStmt(ResolveDeclsContext &context, DoStmt *doStmt) {
    processStmt(context, doStmt->loopStmt);
    processExpr(context, doStmt->condition);
}

void DeclResolverPass::processForStmt(ResolveDeclsContext &context, ForStmt *forStmt) {
    if (forStmt->preLoop != nullptr) processExpr(context, forStmt->preLoop);
    if (forStmt->condition != nullptr) processExpr(context, forStmt->condition);
    if (forStmt->iterationExpr != nullptr) processExpr(context, forStmt->iterationExpr);

    processStmt(context, forStmt->loopStmt);
}

void DeclResolverPass::processGotoStmt(ResolveDeclsContext &context, GotoStmt *gotoStmt) {
    if (!gotoStmt->label().empty()) {
        context.addUnresolvedLabel(gotoStmt->label());
    }
}

void DeclResolverPass::processIfStmt(ResolveDeclsContext &context, IfStmt *ifStmt) {
    processExpr(context, ifStmt->condition);
    processStmt(context, ifStmt->trueStmt);
    if (ifStmt->hasFalseStmt()) processStmt(context, ifStmt->falseStmt);
}

void DeclResolverPass::processLabeledStmt(ResolveDeclsContext &context, LabeledStmt *labeledStmt) {
    processStmt(context, labeledStmt->labeledStmt);

    context.labelResolved(labeledStmt->label());
}

void DeclResolverPass::processReturnStmt(ResolveDeclsContext &context, ReturnStmt *returnStmt) {
    if (returnStmt->hasReturnValue()) {
        processExpr(context, returnStmt->returnValue);

        if (!getTypesAreSame(returnStmt->returnValue->resultType, context.returnType)) {
            // TODO: Check if `returnValueType` can be implicitly casted to the `returnType`
            printError("return value type does not match the function return type!", context.fileAst,
                       returnStmt->returnValue->startPosition(), returnStmt->returnValue->endPosition());
            return;
        }
    }
}

void DeclResolverPass::processSwitchStmt(ResolveDeclsContext &context, SwitchStmt *switchStmt) {
    // TODO: Should we add the type of 'SwitchStmt::condition' to the context?
    processExpr(context, switchStmt->condition);

    for (CaseStmt* caseStmt : switchStmt->cases()) {
        processCaseStmt(context, caseStmt);
    }
}

void DeclResolverPass::processTryStmt(ResolveDeclsContext &context, TryStmt *tryStmt) {
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

void DeclResolverPass::processTryCatchStmt(ResolveDeclsContext &context, TryCatchStmt *tryCatchStmt) {
    if (tryCatchStmt->hasExceptionDecl()) {
        processExpr(context, tryCatchStmt->exceptionType);
    }

    processCompoundStmt(context, tryCatchStmt->handlerStmt);
}

void DeclResolverPass::processTryFinallyStmt(ResolveDeclsContext &context, TryFinallyStmt *tryFinallyStmt) {
    processCompoundStmt(context, tryFinallyStmt->handlerStmt);
}

void DeclResolverPass::processWhileStmt(ResolveDeclsContext &context, WhileStmt *whileStmt) {
    processExpr(context, whileStmt->condition);
    processStmt(context, whileStmt->loopStmt);
}

// Exprs
void DeclResolverPass::processBinaryOperatorExpr(ResolveDeclsContext &context, BinaryOperatorExpr *binaryOperatorExpr) {
    // TODO: Support operator overloading type resolution
    processExpr(context, binaryOperatorExpr->leftValue);
    processExpr(context, binaryOperatorExpr->rightValue);

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
                        context.fileAst, binaryOperatorExpr->startPosition(), binaryOperatorExpr->endPosition());
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

void DeclResolverPass::processCharacterLiteralExpr(ResolveDeclsContext &context, CharacterLiteralExpr *characterLiteralExpr) {
    // TODO: Type suffix support
    characterLiteralExpr->resultType = new BuiltInType({}, {}, "char");
}

void DeclResolverPass::processExplicitCastExpr(ResolveDeclsContext &context, ExplicitCastExpr *explicitCastExpr) {
    explicitCastExpr->resultType = deepCopyAndSimplifyType(explicitCastExpr->castType);
}

void DeclResolverPass::processFloatLiteralExpr(ResolveDeclsContext &context, FloatLiteralExpr *floatLiteralExpr) {
    // TODO: Type suffix support
    floatLiteralExpr->resultType = new BuiltInType({}, {}, "float");
}

void DeclResolverPass::processFunctionCallExpr(ResolveDeclsContext &context, FunctionCallExpr *functionCallExpr) {
    // TODO: Support overloading the operator ()
    for (Expr*& arg : functionCallExpr->arguments) {
        processExpr(context, arg);

        convertLValueToRValue(arg);
    }

    if (functionCallExpr->hasArguments()) {
        context.functionCallArgs = &functionCallExpr->arguments;
    }

    // We process the function reference expression to HOPEFULLY have `functionCallExpr->resultType` be a `FunctionPointerType` if it isn't then we error
    processExpr(context, functionCallExpr->functionReference);

    // TODO: Support `ConstType`, `MutType`, and `ImmutType` since they can all contain `FunctionPointerType`
    if (llvm::isa<FunctionPointerType>(functionCallExpr->functionReference->resultType)) {
        auto functionPointerType = llvm::dyn_cast<FunctionPointerType>(functionCallExpr->functionReference->resultType);

        // We set the result type of this expression to the result type of the function being called.
        functionCallExpr->resultType = deepCopyAndSimplifyType(functionPointerType->resultType);
    } else {
        printError("expression is not a valid function reference!",
                   context.fileAst,
                   functionCallExpr->functionReference->startPosition(),
                   functionCallExpr->functionReference->endPosition());
    }

    context.functionCallArgs = nullptr;
}

/**
 * Try to find the `IdentifierExpr` within the current context by searching local variables, params, decls, etc.
 */
void DeclResolverPass::processIdentifierExpr(ResolveDeclsContext &context, IdentifierExpr *identifierExpr) {
    if (identifierExpr->hasTemplateArguments()) {
        printError("templating support currently not finished!",
                   context.fileAst, identifierExpr->startPosition(), identifierExpr->endPosition());
        return;
    }

    // We check local variables first
    for (std::size_t i = 0; i < context.functionLocalVariablesCount; ++i) {
        if (context.functionLocalVariables[i]->name() == identifierExpr->name()) {
            identifierExpr->resultType = deepCopyAndSimplifyType(context.functionLocalVariables[i]->resultType);
            return;
        }
    }

    // then params
    if (context.functionParams != nullptr) {
        for (const ParameterDecl* param : *context.functionParams) {
            if (param->name() == identifierExpr->name()) {
                identifierExpr->resultType = deepCopyAndSimplifyType(param->type);
                return;
            }
        }
    }

    // then function template params
    if (context.functionTemplateParams != nullptr) {
        for (const TemplateParameterDecl* templateParam : *context.functionTemplateParams) {
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

    for (const Decl* decl : context.fileAst.topLevelDecls()) {
        if (decl->name() == identifierExpr->name()) {
            // This is currently the only `Decl` we support at the top level
            if (llvm::isa<FunctionDecl>(decl)) {
                // TODO: We need to set something in the context to allow us to know what the function calls current parameters are.
                auto functionDecl = llvm::dyn_cast<FunctionDecl>(decl);

                if (functionDecl->name() == identifierExpr->name()) {
                    if (!functionDecl->hasParameters()) {
                        // If the function doesn't have parameters and there aren't any arguments then we've found a perfect match...
                        if ((context.functionCallArgs == nullptr || context.functionCallArgs->empty())) {
                            // It is a perfect match, set the foundFunction variable so we can create the `FunctionPointerType`
                            foundFunction = functionDecl;
                            goto functionWasFound; // This is why we support breaking from labeled loops...
                        } else {
                            // Args don't match params at all, keep looking.
                            continue;
                        }
                    } else { // Else the function decl has parameters...
                        // If the function call doesn't have args then it doesn't match...
                        if ((context.functionCallArgs == nullptr || context.functionCallArgs->empty())) {
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

                        for (; i < context.functionCallArgs->size(); ++i) {
                            if (i >= functionDecl->parameters.size()) {
                                argsMatch = false;
                                break;
                            }

                            // TODO: Support checking if something can be implicitly casted properly.
                            if (!getTypesAreSame((*(context.functionCallArgs))[i]->resultType, functionDecl->resultType)) {
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
               context.fileAst, identifierExpr->startPosition(), identifierExpr->endPosition());
}

void DeclResolverPass::processImplicitCastExpr(ResolveDeclsContext &context, ImplicitCastExpr *implicitCastExpr) {
    if (implicitCastExpr->resultType == nullptr) {
        implicitCastExpr->resultType = (deepCopyAndSimplifyType(implicitCastExpr->castType));
    }
}

void DeclResolverPass::processIndexerCallExpr(ResolveDeclsContext &context, IndexerCallExpr *indexerCallExpr) {
    // TODO: Support overloading the indexer operator []
    printError("array indexer calls not yet supported!", context.fileAst, indexerCallExpr->startPosition(), indexerCallExpr->endPosition());
}

void DeclResolverPass::processIntegerLiteralExpr(ResolveDeclsContext &context, IntegerLiteralExpr *integerLiteralExpr) {
    // TODO: Type suffix support
    integerLiteralExpr->resultType = (new BuiltInType({}, {}, "int"));
}

void DeclResolverPass::processLocalVariableDeclExpr(ResolveDeclsContext &context, LocalVariableDeclExpr *localVariableDeclExpr) {
    if (llvm::isa<ResolvedTypeRefExpr>(localVariableDeclExpr->type())) {
        if (context.localVariableNameTaken(localVariableDeclExpr->name())) {
            printError("redefinition of variable '" + localVariableDeclExpr->name() + "' not allowed!",
                       context.fileAst, localVariableDeclExpr->startPosition(),
                       localVariableDeclExpr->endPosition());
        }

        auto resolvedTypeRefExpr = llvm::dyn_cast<ResolvedTypeRefExpr>(localVariableDeclExpr->type());
        localVariableDeclExpr->resultType = deepCopyAndSimplifyType(resolvedTypeRefExpr->resolvedType());

        context.addLocalVariable(localVariableDeclExpr);
    }
}

void DeclResolverPass::processLocalVariableDeclOrPrefixOperatorCallExpr(ResolveDeclsContext &context, Expr *&expr) {
    // We don't have anything to do here...
}

void DeclResolverPass::processMemberAccessCallExpr(ResolveDeclsContext &context, MemberAccessCallExpr *memberAccessCallExpr) {
    // TODO: Support operator overloading the `.` and `->` operators
    // TODO: `public override T operator.() => return this.whatever_T_is;` this can ONLY be supported when the implementing class/struct has NO public facing functions
    printError("member access calls not yet supported!", context.fileAst, memberAccessCallExpr->startPosition(), memberAccessCallExpr->endPosition());
}

void DeclResolverPass::processParenExpr(ResolveDeclsContext &context, ParenExpr *parenExpr) {
    processExpr(context, parenExpr->containedExpr);

    parenExpr->resultType = deepCopyAndSimplifyType(parenExpr->containedExpr->resultType);

    convertLValueToRValue(parenExpr->containedExpr);
}

void DeclResolverPass::processPostfixOperatorExpr(ResolveDeclsContext &context, PostfixOperatorExpr *postfixOperatorExpr) {
    // TODO: Support operator overloading type resolution
    processExpr(context, postfixOperatorExpr->expr);

    postfixOperatorExpr->resultType = deepCopyAndSimplifyType(postfixOperatorExpr->expr->resultType);
}

void DeclResolverPass::processPotentialExplicitCastExpr(ResolveDeclsContext &context, PotentialExplicitCastExpr *potentialExplicitCastExpr) {

}

void DeclResolverPass::processPrefixOperatorExpr(ResolveDeclsContext &context, PrefixOperatorExpr *prefixOperatorExpr) {
    processExpr(context, prefixOperatorExpr->expr);

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
                       context.fileAst, prefixOperatorExpr->startPosition(), prefixOperatorExpr->endPosition());
            return;
        }
    } else {
        prefixOperatorExpr->resultType = deepCopyAndSimplifyType(prefixOperatorExpr->expr->resultType);
    }
}

void DeclResolverPass::processResolvedTypeRefExpr(ResolveDeclsContext &context, ResolvedTypeRefExpr *resolvedTypeRefExpr) {

}

void DeclResolverPass::processStringLiteralExpr(ResolveDeclsContext &context, StringLiteralExpr *stringLiteralExpr) {
    // TODO: Type suffix support
    stringLiteralExpr->resultType = (new PointerType({}, {}, new BuiltInType({}, {}, "char")));
}

void DeclResolverPass::processTernaryExpr(ResolveDeclsContext &context, TernaryExpr *ternaryExpr) {

}

void DeclResolverPass::processUnresolvedTypeRefExpr(ResolveDeclsContext &context, Expr *&expr) {
    printError("type not found!", context.fileAst, expr->startPosition(), expr->endPosition());
}

void DeclResolverPass::convertLValueToRValue(Expr*& potentialLValue) {
    // TODO: IdentifierExpr isn't the only thing we will have to do this for, right?
    if (llvm::isa<IdentifierExpr>(potentialLValue)) {
        Expr* newRValue = new LValueToRValueExpr(potentialLValue->startPosition(), potentialLValue->endPosition(), potentialLValue);
        newRValue->resultType = deepCopyAndSimplifyType(potentialLValue->resultType);
        potentialLValue = newRValue;
    }
}
