#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/ConstantFolder.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Verifier.h"

#include <AST/Types/BuiltInType.hpp>
#include <AST/Types/PointerType.hpp>
#include <AST/Exprs/ParenExpr.hpp>
#include "CodeGen.hpp"

void gulc::CodeGen::generate(gulc::FileAST& file) {
    llvm::LLVMContext llvmContext;
    llvm::IRBuilder<> irBuilder(llvmContext);
    // TODO: Should we remove the ending file extension?
    llvm::Module* genModule = new llvm::Module(file.filePath(), llvmContext);

    gulc::CodeGenContext codeGenContext = CodeGenContext(file, llvmContext, irBuilder, genModule);

    for (const gulc::Decl* decl : file.topLevelDecls()) {
        generateDecl(codeGenContext, decl);

        //globalObject.print()
    }

    genModule->print(llvm::errs(), nullptr);

    // TODO: Should we get rid of all `verifyFunction` calls and replace it with `verifyModule` here?
}

void gulc::CodeGen::printError(const std::string &message, gulc::FileAST &fileAst, TextPosition startPosition, TextPosition endPosition) {
    std::cout << "gulc codegen error[" << fileAst.filePath() << ", "
                                    "{" << startPosition.line << ", " << startPosition.column << "} "
                                    "to {" << endPosition.line << ", " << endPosition.column << "}]: "
              << message
              << std::endl;
    std::exit(1);
}

llvm::Type *gulc::CodeGen::generateLlvmType(gulc::CodeGenContext& context, const gulc::Type* type) {
    switch (type->getTypeKind()) {
        case gulc::Type::Kind::BuiltIn: {
            auto builtInType = llvm::dyn_cast<gulc::BuiltInType>(type);

            // Bool is special...
            if (builtInType->isBool()) {
                return llvm::Type::getInt1Ty(context.llvmContext);
            }

            // Floats...
            if (builtInType->isFloating()) {
                switch (builtInType->size()) {
                    case 2:
                        return llvm::Type::getHalfTy(context.llvmContext);
                    case 4:
                        return llvm::Type::getFloatTy(context.llvmContext);
                    case 8:
                        return llvm::Type::getDoubleTy(context.llvmContext);
                    //case 16: 128 bit is support on some platforms, we should support it...
                    default:
                        printError("unsupported floating point size for type '" + type->getString() + "'!",
                                   context.fileAst, type->startPosition(), type->endPosition());
                        return nullptr;
                }
            }

            // Signed/unsigned ints...
            // Thing to not here is that LLVM handles the signedness of a variable by the operation you used, similar to assembly. That is why we don't check if it is signed here.
            switch (builtInType->size()) {
                case 1:
                    return llvm::Type::getInt8Ty(context.llvmContext);
                case 2:
                    return llvm::Type::getInt16Ty(context.llvmContext);
                case 4:
                    return llvm::Type::getInt32Ty(context.llvmContext);
                case 8:
                    return llvm::Type::getInt64Ty(context.llvmContext);
                    //case 16: 128 bit is supported on some platforms, we should support it...
                default:
                    printError("unsupported floating point size for type '" + type->getString() + "'!",
                               context.fileAst, type->startPosition(), type->endPosition());
                    return nullptr;
            }
        }
        case gulc::Type::Kind::Pointer: {
            auto pointerType = llvm::dyn_cast<gulc::PointerType>(type);
            // TODO: Is this right? What is the address space stuff?
            return llvm::PointerType::getUnqual(generateLlvmType(context, pointerType->pointToType));
        }
        default:
            printError("type '" + type->getString() + "' not yet supported!",
                       context.fileAst, type->startPosition(), type->endPosition());
            return nullptr;
    }
    return nullptr;
}

std::vector<llvm::Type*> gulc::CodeGen::generateParamTypes(gulc::CodeGenContext& context, const std::vector<ParameterDecl*>& parameters) {
    std::vector<llvm::Type*> paramTypes{};
    paramTypes.reserve(parameters.size());

    for (const ParameterDecl* parameterDecl : parameters) {
        paramTypes.push_back(generateLlvmType(context, parameterDecl->type));
    }

    return paramTypes;
}

llvm::GlobalObject* gulc::CodeGen::generateDecl(gulc::CodeGenContext& context, const gulc::Decl *decl) {
    switch (decl->getDeclKind()) {
        case gulc::Decl::Kind::Function:
            return generateFunctionDecl(context, llvm::dyn_cast<gulc::FunctionDecl>(decl));
        default:
            printError("internal - unsupported decl!",
                       context.fileAst, decl->startPosition(), decl->endPosition());
            break;
    }

	return nullptr;
}

void gulc::CodeGen::generateStmt(gulc::CodeGenContext &context, const gulc::Stmt *stmt) {
    switch (stmt->getStmtKind()) {
        case gulc::Stmt::Kind::Break:
            printError("`break` not yet supported!", context.fileAst, stmt->startPosition(), stmt->endPosition());
            return;
        case gulc::Stmt::Kind::Case:
            printError("`case` not yet supported!", context.fileAst, stmt->startPosition(), stmt->endPosition());
            return;
        case gulc::Stmt::Kind::Compound:
            return generateCompoundStmt(context, llvm::dyn_cast<CompoundStmt>(stmt));
        case gulc::Stmt::Kind::Continue:
            printError("`continue` not yet supported!", context.fileAst, stmt->startPosition(), stmt->endPosition());
            return;
        case gulc::Stmt::Kind::Do:
            printError("`do` not yet supported!", context.fileAst, stmt->startPosition(), stmt->endPosition());
            return;
        case gulc::Stmt::Kind::For:
            printError("`for` not yet supported!", context.fileAst, stmt->startPosition(), stmt->endPosition());
            return;
        case gulc::Stmt::Kind::Goto:
            return generateGotoStmt(context, llvm::dyn_cast<GotoStmt>(stmt));
        case gulc::Stmt::Kind::If:
            printError("`if` not yet supported!", context.fileAst, stmt->startPosition(), stmt->endPosition());
            return;
        case gulc::Stmt::Kind::Labeled:
            return generateLabeledStmt(context, llvm::dyn_cast<LabeledStmt>(stmt));
        case gulc::Stmt::Kind::Return:
            return generateReturnStmt(context, llvm::dyn_cast<ReturnStmt>(stmt));
        case gulc::Stmt::Kind::Switch:
            printError("`switch` not yet supported!", context.fileAst, stmt->startPosition(), stmt->endPosition());
            return;
        case gulc::Stmt::Kind::Try:
            printError("`try` not yet supported!", context.fileAst, stmt->startPosition(), stmt->endPosition());
            return;
        case gulc::Stmt::Kind::TryCatch:
            printError("`catch` not yet supported!", context.fileAst, stmt->startPosition(), stmt->endPosition());
            return;
        case gulc::Stmt::Kind::TryFinally:
            printError("`finally` not yet supported!", context.fileAst, stmt->startPosition(), stmt->endPosition());
            return;
        case gulc::Stmt::Kind::While:
            printError("`while` not yet supported!", context.fileAst, stmt->startPosition(), stmt->endPosition());
            return;
        case gulc::Stmt::Kind::Expr:
            // TODO: Is this a memory leak? I can't figure out if we have to explicitly free the unused `Value*` or if LLVM is storing it somewhere else?
            //  I even went so far as to check the `clang` code generator and they explicitly state they're ignoring an `LValue` result...
            //  and when I go to the definition of `LValue` it has an `llvm::Value*` member that isn't freed in a constructor...
            generateExpr(context, llvm::dyn_cast<Expr>(stmt));
            break;
    }
}

llvm::Value* gulc::CodeGen::generateExpr(CodeGenContext& context, const Expr* expr) {
    switch (expr->getExprKind()) {
        case gulc::Expr::Kind::BinaryOperator:
            return generateBinaryOperatorExpr(context, llvm::dyn_cast<BinaryOperatorExpr>(expr));
        case gulc::Expr::Kind::CharacterLiteral:
            printError("character literals not yet supported!",
                       context.fileAst, expr->startPosition(), expr->endPosition());
            return nullptr;
        case gulc::Expr::Kind::ExplicitCast:
            printError("explicit casts not yet supported!",
                       context.fileAst, expr->startPosition(), expr->endPosition());
            return nullptr;
        case gulc::Expr::Kind::FloatLiteral:
            return generateFloatLiteralExpr(context, llvm::dyn_cast<FloatLiteralExpr>(expr));
        case gulc::Expr::Kind::FunctionCall:
            printError("function calls not yet supported!",
                       context.fileAst, expr->startPosition(), expr->endPosition());
            return nullptr;
        case gulc::Expr::Kind::Identifier:
            printError("identifiers not yet supported!",
                       context.fileAst, expr->startPosition(), expr->endPosition());
            return nullptr;
        case gulc::Expr::Kind::ImplicitCast:
            printError("implicit casts not yet supported!",
                       context.fileAst, expr->startPosition(), expr->endPosition());
            return nullptr;
        case gulc::Expr::Kind::IndexerCall:
            printError("indexer calls not yet supported!",
                       context.fileAst, expr->startPosition(), expr->endPosition());
            return nullptr;
        case gulc::Expr::Kind::IntegerLiteral:
            return generateIntegerLiteralExpr(context, llvm::dyn_cast<IntegerLiteralExpr>(expr));
        case gulc::Expr::Kind::LocalVariableDecl:
            printError("local variable declarations not yet supported!",
                       context.fileAst, expr->startPosition(), expr->endPosition());
            return nullptr;
        case gulc::Expr::Kind::MemberAccessCall:
            printError("member access calls not yet supported!",
                       context.fileAst, expr->startPosition(), expr->endPosition());
            return nullptr;
        case gulc::Expr::Kind::Paren:
            return generateExpr(context, llvm::dyn_cast<ParenExpr>(expr)->containedExpr);
        case gulc::Expr::Kind::PostfixOperator:
            printError("postfix operators not yet supported!",
                       context.fileAst, expr->startPosition(), expr->endPosition());
            return nullptr;
        case gulc::Expr::Kind::PrefixOperator:
            printError("prefix operators not yet supported!",
                       context.fileAst, expr->startPosition(), expr->endPosition());
            return nullptr;
        case gulc::Expr::Kind::StringLiteral:
            printError("string literals not yet supported!",
                       context.fileAst, expr->startPosition(), expr->endPosition());
            return nullptr;
        case gulc::Expr::Kind::Ternary:
            printError("ternary operators not yet supported!",
                       context.fileAst, expr->startPosition(), expr->endPosition());
            return nullptr;
        default:
            printError("unexpected expression type in code generator!",
                       context.fileAst, expr->startPosition(), expr->endPosition());
            return nullptr;
    }
}

// Helpers
void gulc::CodeGen::addBlockAndSetInsertionPoint(CodeGenContext& context, llvm::BasicBlock* basicBlock) {
    context.currentFunction->getBasicBlockList().push_back(basicBlock);
    context.irBuilder.SetInsertPoint(basicBlock);
}

// Decls
llvm::Function* gulc::CodeGen::generateFunctionDecl(gulc::CodeGenContext& context, const gulc::FunctionDecl *functionDecl) {
    std::vector<llvm::Type*> paramTypes = generateParamTypes(context, functionDecl->parameters);
    llvm::Type* returnType = generateLlvmType(context, functionDecl->resultType);
    llvm::FunctionType* functionType = llvm::FunctionType::get(returnType, paramTypes, false);
    llvm::Function* function = llvm::Function::Create(functionType, llvm::Function::PrivateLinkage, functionDecl->name(), context.module);

    llvm::BasicBlock* funcBody = llvm::BasicBlock::Create(context.llvmContext, "entry", function);
    context.irBuilder.SetInsertPoint(funcBody);
	context.currentFunction = function;

    // Generate the function body
    generateStmt(context, functionDecl->body());

    // TODO: We might want to remove this.
    verifyFunction(*function);

    // Reset the insertion point (this probably isn't needed but oh well)
    context.irBuilder.ClearInsertionPoint();
	context.currentFunction = nullptr;

    return function;
}

// Stmts
void gulc::CodeGen::generateCompoundStmt(gulc::CodeGenContext &context, const gulc::CompoundStmt* compoundStmt) {
    for (const Stmt* stmt : compoundStmt->statements()) {
        generateStmt(context, stmt);
    }
}

void gulc::CodeGen::generateReturnStmt(gulc::CodeGenContext &context, const gulc::ReturnStmt *returnStmt) {
    if (returnStmt->hasReturnValue()) {
        context.irBuilder.CreateRet(generateExpr(context, returnStmt->returnValue));
        return;
    } else {
        context.irBuilder.CreateRetVoid();
        return;
    }
}

// Exprs
llvm::Value* gulc::CodeGen::generateBinaryOperatorExpr(gulc::CodeGenContext &context, const gulc::BinaryOperatorExpr *binaryOperatorExpr) {
    llvm::Value* leftValue = generateExpr(context, binaryOperatorExpr->leftValue);
    llvm::Value* rightValue = generateExpr(context, binaryOperatorExpr->rightValue);

    // TODO: Support the `PointerType`
    bool isFloat = false;
    bool isSigned = false;

    if (llvm::isa<BuiltInType>(binaryOperatorExpr->resultType)) {
        auto builtInType = llvm::dyn_cast<BuiltInType>(binaryOperatorExpr->resultType);

        isFloat = builtInType->isFloating();
        isSigned = builtInType->isSigned();
    } else if (!llvm::isa<PointerType>(binaryOperatorExpr->resultType)) {
        printError("unknown binary operator expression!",
                   context.fileAst, binaryOperatorExpr->startPosition(), binaryOperatorExpr->endPosition());
        return nullptr;
    }

    if (binaryOperatorExpr->operatorName() == "+") {
        if (isFloat) {
            return context.irBuilder.CreateFAdd(leftValue, rightValue, "addtmp");
        } else {
            return context.irBuilder.CreateAdd(leftValue, rightValue, "addtmp");
        }
    } else if (binaryOperatorExpr->operatorName() == "-") {
        if (isFloat) {
            return context.irBuilder.CreateFSub(leftValue, rightValue, "subtmp");
        } else {
            return context.irBuilder.CreateSub(leftValue, rightValue, "subtmp");
        }
    } else if (binaryOperatorExpr->operatorName() == "*") {
        if (isFloat) {
            return context.irBuilder.CreateFMul(leftValue, rightValue, "multmp");
        } else {
            return context.irBuilder.CreateMul(leftValue, rightValue, "multmp");
        }
    } else if (binaryOperatorExpr->operatorName() == "/") {
        if (isFloat) {
            return context.irBuilder.CreateFDiv(leftValue, rightValue, "divtmp");
        } else {
            // TODO: What are the `Exact` variants?
            if (isSigned) {
                return context.irBuilder.CreateSDiv(leftValue, rightValue, "divtmp");
            } else {
                return context.irBuilder.CreateUDiv(leftValue, rightValue, "divtmp");
            }
        }
    } else if (binaryOperatorExpr->operatorName() == "%") {
        if (isFloat) {
            return context.irBuilder.CreateFRem(leftValue, rightValue, "remtmp");
        } else {
            // TODO: What are the `Exact` variants?
            if (isSigned) {
                return context.irBuilder.CreateSRem(leftValue, rightValue, "remtmp");
            } else {
                return context.irBuilder.CreateURem(leftValue, rightValue, "remtmp");
            }
        }
    } else if (binaryOperatorExpr->operatorName() == "^") {
        return context.irBuilder.CreateXor(leftValue, rightValue, "xortmp");
    } else if (binaryOperatorExpr->operatorName() == "<<") {
        return context.irBuilder.CreateShl(leftValue, rightValue, "shltmp");
    } else if (binaryOperatorExpr->operatorName() == ">>") {
        if (isSigned) {
            return context.irBuilder.CreateAShr(leftValue, rightValue, "ashrtmp");
        } else {
            return context.irBuilder.CreateLShr(leftValue, rightValue, "lshrtmp");
        }
    } else if (binaryOperatorExpr->operatorName() == "==") {
        if (isFloat) {
            return context.irBuilder.CreateFCmpOEQ(leftValue, rightValue, "eqtmp");
        } else {
            return context.irBuilder.CreateICmpEQ(leftValue, rightValue, "eqtmp");
        }
    } else if (binaryOperatorExpr->operatorName() == ">") {
        if (isFloat) {
            return context.irBuilder.CreateFCmpOGT(leftValue, rightValue, "gttmp");
        } else {
            if (isSigned) {
                return context.irBuilder.CreateICmpSGT(leftValue, rightValue, "gttmp");
            } else {
                return context.irBuilder.CreateICmpUGT(leftValue, rightValue, "gttmp");
            }
        }
    } else if (binaryOperatorExpr->operatorName() == ">=") {
        if (isFloat) {
            return context.irBuilder.CreateFCmpOGE(leftValue, rightValue, "getmp");
        } else {
            if (isSigned) {
                return context.irBuilder.CreateICmpSGE(leftValue, rightValue, "getmp");
            } else {
                return context.irBuilder.CreateICmpUGE(leftValue, rightValue, "getmp");
            }
        }
    } else if (binaryOperatorExpr->operatorName() == "<") {
        if (isFloat) {
            return context.irBuilder.CreateFCmpOLT(leftValue, rightValue, "lttmp");
        } else {
            if (isSigned) {
                return context.irBuilder.CreateICmpSLT(leftValue, rightValue, "lttmp");
            } else {
                return context.irBuilder.CreateICmpULT(leftValue, rightValue, "lttmp");
            }
        }
    } else if (binaryOperatorExpr->operatorName() == "<=") {
        if (isFloat) {
            return context.irBuilder.CreateFCmpOLE(leftValue, rightValue, "letmp");
        } else {
            if (isSigned) {
                return context.irBuilder.CreateICmpSLE(leftValue, rightValue, "letmp");
            } else {
                return context.irBuilder.CreateICmpULE(leftValue, rightValue, "letmp");
            }
        }
    } else {
        printError("binary operator '" + binaryOperatorExpr->operatorName() + "' not yet supported!",
                   context.fileAst, binaryOperatorExpr->startPosition(), binaryOperatorExpr->endPosition());
        return nullptr;
    }
}

llvm::Value *gulc::CodeGen::generateIntegerLiteralExpr(gulc::CodeGenContext &context, const gulc::IntegerLiteralExpr *integerLiteralExpr) {
    if (auto builtInType = llvm::dyn_cast<BuiltInType>(integerLiteralExpr->resultType)) {
        unsigned int numOfBits = builtInType->size() * 8;
        return llvm::ConstantInt::get(context.llvmContext, llvm::APInt(numOfBits, integerLiteralExpr->numberString(), integerLiteralExpr->numberBase()));
    } else {
        printError("unknown integer literal type!",
                   context.fileAst, integerLiteralExpr->startPosition(), integerLiteralExpr->endPosition());
        return nullptr;
    }
}

llvm::Value *gulc::CodeGen::generateFloatLiteralExpr(gulc::CodeGenContext &context, const gulc::FloatLiteralExpr *floatLiteralExpr) {
    if (auto builtInType = llvm::dyn_cast<BuiltInType>(floatLiteralExpr->resultType)) {
        switch (builtInType->size()) {
            case 2:
                return llvm::ConstantFP::get(context.llvmContext, llvm::APFloat(llvm::APFloat::IEEEhalf(), floatLiteralExpr->numberValue()));
            case 4:
                return llvm::ConstantFP::get(context.llvmContext, llvm::APFloat(llvm::APFloat::IEEEsingle(), floatLiteralExpr->numberValue()));
            case 8:
                return llvm::ConstantFP::get(context.llvmContext, llvm::APFloat(llvm::APFloat::IEEEdouble(), floatLiteralExpr->numberValue()));
            // case 16: // TODO: Support the quad
            default:
                printError("unsupported floating point size!",
                           context.fileAst, floatLiteralExpr->startPosition(), floatLiteralExpr->endPosition());
                return nullptr;
        }
    }

    printError("unknown float literal type!",
               context.fileAst, floatLiteralExpr->startPosition(), floatLiteralExpr->endPosition());
    return nullptr;
}

void gulc::CodeGen::generateLabeledStmt(gulc::CodeGenContext &context, const gulc::LabeledStmt *labeledStmt) {
    // Curious why we can't just use normal labels...
    llvm::BasicBlock* labelBody;

    if (context.currentFunctionLabelsContains(labeledStmt->label())) {
        labelBody = context.currentFunctionLabels[labeledStmt->label()];
    } else {
        labelBody = llvm::BasicBlock::Create(context.llvmContext, labeledStmt->label());
    }

    addBlockAndSetInsertionPoint(context, labelBody);

    if (labeledStmt->labeledStmt != nullptr) {
        generateStmt(context, labeledStmt->labeledStmt);
    }
}

void gulc::CodeGen::generateGotoStmt(gulc::CodeGenContext &context, const gulc::GotoStmt *gotoStmt) {
    if (context.currentFunctionLabelsContains(gotoStmt->label())) {
        context.irBuilder.CreateBr(context.currentFunctionLabels[gotoStmt->label()]);
    } else {
        llvm::BasicBlock* newBasicBlock = llvm::BasicBlock::Create(context.llvmContext, gotoStmt->label());
        context.irBuilder.CreateBr(newBasicBlock);
    }
}
