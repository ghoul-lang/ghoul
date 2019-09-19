#include "llvm/ADT/APFloat.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Transforms/Utils.h"
#include "llvm/Transforms/InstCombine/InstCombine.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Transforms/Scalar/GVN.h"

#include <AST/Types/BuiltInType.hpp>
#include <AST/Types/PointerType.hpp>
#include <AST/Exprs/ParenExpr.hpp>
#include "CodeGen.hpp"

gulc::Module gulc::CodeGen::generate(gulc::FileAST& file) {
    llvm::LLVMContext llvmContext;
    llvm::IRBuilder<> irBuilder(llvmContext);
    // TODO: Should we remove the ending file extension?
    llvm::Module* genModule = new llvm::Module(file.filePath(), llvmContext);
    //llvm::PassManager<llvm::Function>* funcPassManager = new llvm::PassManager<llvm::Function>();
    llvm::legacy::FunctionPassManager* funcPassManager = new llvm::legacy::FunctionPassManager(genModule);

    // Promote allocas to registers.
    funcPassManager->add(llvm::createPromoteMemoryToRegisterPass());
    // Do simple "peephole" optimizations and bit-twiddling optzns.
    funcPassManager->add(llvm::createInstructionCombiningPass());
    // Reassociate expressions.
    funcPassManager->add(llvm::createReassociatePass());
    // Eliminate Common SubExpressions.
    funcPassManager->add(llvm::createGVNPass());
    // Simplify the control flow graph (deleting unreachable blocks, etc).
    funcPassManager->add(llvm::createCFGSimplificationPass());

    funcPassManager->doInitialization();

    gulc::CodeGenContext codeGenContext = CodeGenContext(file, llvmContext, irBuilder, genModule, funcPassManager);

    for (const gulc::Decl* decl : file.topLevelDecls()) {
        generateDecl(codeGenContext, decl);

        //globalObject.print()
    }

    //genModule->print(llvm::errs(), nullptr);

    funcPassManager->doFinalization();
    delete funcPassManager;

    return Module(file.filePath(), genModule);
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

void gulc::CodeGen::generateStmt(gulc::CodeGenContext &context, const gulc::Stmt *stmt, const std::string& stmtName) {
    switch (stmt->getStmtKind()) {
        case gulc::Stmt::Kind::Break:
            return generateBreakStmt(context, llvm::dyn_cast<BreakStmt>(stmt));
        case gulc::Stmt::Kind::Case:
            printError("`case` not yet supported!", context.fileAst, stmt->startPosition(), stmt->endPosition());
            return;
        case gulc::Stmt::Kind::Compound:
            return generateCompoundStmt(context, llvm::dyn_cast<CompoundStmt>(stmt));
        case gulc::Stmt::Kind::Continue:
            return generateContinueStmt(context, llvm::dyn_cast<ContinueStmt>(stmt));
        case gulc::Stmt::Kind::Do:
            return generateDoStmt(context, llvm::dyn_cast<DoStmt>(stmt), stmtName);
        case gulc::Stmt::Kind::For:
            return generateForStmt(context, llvm::dyn_cast<ForStmt>(stmt), stmtName);
        case gulc::Stmt::Kind::Goto:
            return generateGotoStmt(context, llvm::dyn_cast<GotoStmt>(stmt));
        case gulc::Stmt::Kind::If:
            return generateIfStmt(context, llvm::dyn_cast<IfStmt>(stmt));
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
            return generateWhileStmt(context, llvm::dyn_cast<WhileStmt>(stmt), stmtName);
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
            return generateFunctionCallExpr(context, llvm::dyn_cast<FunctionCallExpr>(expr));
        case gulc::Expr::Kind::Identifier:
            return generateIdentifierExpr(context, llvm::dyn_cast<IdentifierExpr>(expr));
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
            return generateLocalVariableDeclExpr(context, llvm::dyn_cast<LocalVariableDeclExpr>(expr));
        case gulc::Expr::Kind::MemberAccessCall:
            printError("member access calls not yet supported!",
                       context.fileAst, expr->startPosition(), expr->endPosition());
            return nullptr;
        case gulc::Expr::Kind::Paren:
            return generateExpr(context, llvm::dyn_cast<ParenExpr>(expr)->containedExpr);
        case gulc::Expr::Kind::PostfixOperator:
            return generatePostfixOperatorExpr(context, llvm::dyn_cast<PostfixOperatorExpr>(expr));
        case gulc::Expr::Kind::PrefixOperator:
            return generatePrefixOperatorExpr(context, llvm::dyn_cast<PrefixOperatorExpr>(expr));
        case gulc::Expr::Kind::StringLiteral:
            printError("string literals not yet supported!",
                       context.fileAst, expr->startPosition(), expr->endPosition());
            return nullptr;
        case gulc::Expr::Kind::Ternary:
            printError("ternary operators not yet supported!",
                       context.fileAst, expr->startPosition(), expr->endPosition());
            return nullptr;
        case gulc::Expr::Kind::LValueToRValue:
            return generateLValueToRValue(context, llvm::dyn_cast<LValueToRValueExpr>(expr));
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
    context.setCurrentFunction(function);

    // Generate the function body
    context.currentFunctionLocalVariablesCount = 0;
    generateStmt(context, functionDecl->body());
    context.currentFunctionLocalVariablesCount = 0;

    // TODO: We might want to remove this.
    verifyFunction(*function);
    context.funcPass->run(*function);

    // Reset the insertion point (this probably isn't needed but oh well)
    context.irBuilder.ClearInsertionPoint();
	context.currentFunction = nullptr;

    return function;
}

// Stmts
void gulc::CodeGen::generateCompoundStmt(gulc::CodeGenContext &context, const gulc::CompoundStmt* compoundStmt) {
    unsigned int oldLocalVariableCount = context.currentFunctionLocalVariablesCount;

    for (const Stmt* stmt : compoundStmt->statements()) {
        generateStmt(context, stmt);
    }

    context.currentFunctionLocalVariablesCount = oldLocalVariableCount;
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

void gulc::CodeGen::generateIfStmt(gulc::CodeGenContext &context, const gulc::IfStmt *ifStmt) {
    llvm::Value* cond = generateExpr(context, ifStmt->condition);

    llvm::BasicBlock* trueBlock = llvm::BasicBlock::Create(context.llvmContext, "ifTrueBlock", context.currentFunction);
    llvm::BasicBlock* mergeBlock = llvm::BasicBlock::Create(context.llvmContext, "ifMerge");
    llvm::BasicBlock* falseBlock = nullptr;

    // If there isn't a false block we make the IR jump to the merge block on false, else we make an actual false block
    if (ifStmt->hasFalseStmt()) {
        falseBlock = llvm::BasicBlock::Create(context.llvmContext, "ifFalseBlock");

        context.irBuilder.CreateCondBr(cond, trueBlock, falseBlock);
    } else {
        context.irBuilder.CreateCondBr(cond, trueBlock, mergeBlock);
    }

    // Set the insert point to our true block then generate the statement for it...
    context.irBuilder.SetInsertPoint(trueBlock);
    generateStmt(context, ifStmt->trueStmt);
    context.irBuilder.CreateBr(mergeBlock);

    // And then add a jump to the merge block if there is a false statement
    if (ifStmt->hasFalseStmt()) {
        // Set the insert point to the false block and then generate the statement for it...
        context.currentFunction->getBasicBlockList().push_back(falseBlock);
        context.irBuilder.SetInsertPoint(falseBlock);
        generateStmt(context, ifStmt->falseStmt);
        // Branch to merge when done...
        context.irBuilder.CreateBr(mergeBlock);
    }

    // Add the merge block to the function and then set the insert point to it
    context.currentFunction->getBasicBlockList().push_back(mergeBlock);
    context.irBuilder.SetInsertPoint(mergeBlock);
}

void gulc::CodeGen::generateWhileStmt(gulc::CodeGenContext &context, const gulc::WhileStmt *whileStmt, const std::string& loopName) {
    std::string whileName;

    if (loopName.empty()) {
        whileName = "loop" + std::to_string(context.loopNameNumber);
        ++context.loopNameNumber;
    } else {
        whileName = loopName;
    }

    llvm::BasicBlock* continueLoop = llvm::BasicBlock::Create(context.llvmContext, whileName + "_continue", context.currentFunction);
    llvm::BasicBlock* loop = llvm::BasicBlock::Create(context.llvmContext, whileName + "_loop", context.currentFunction);
    llvm::BasicBlock* breakLoop = llvm::BasicBlock::Create(context.llvmContext, whileName + "_break");

    // For some reason we can't just fall through to the continue loop? We have to explicitly branch to it?
    context.irBuilder.CreateBr(continueLoop);

    // Set the insert point to the continue loop block and start adding the loop data...
    context.irBuilder.SetInsertPoint(continueLoop);

    llvm::Value* cond = generateExpr(context, whileStmt->condition);
    // If the condition is true we continue the loop, if not we break from the loop...
    context.irBuilder.CreateCondBr(cond, loop, breakLoop);

    // Set the insert point to the loop block for our actual statement...
    context.irBuilder.SetInsertPoint(loop);

    // We make sure to back up and restore the old loop's break and continue blocks for our `break` and `continue` keywords
    llvm::BasicBlock* oldLoopContinue = context.currentLoopBlockContinue;
    llvm::BasicBlock* oldLoopBreak = context.currentLoopBlockBreak;

    context.currentLoopBlockContinue = continueLoop;
    context.currentLoopBlockBreak = breakLoop;

    // Generate the loop statement within the loop block then jump back to the continue block...
    context.enterNestedLoop(continueLoop, breakLoop);
    generateStmt(context, whileStmt->loopStmt);
    context.leaveNestedLoop();
    context.irBuilder.CreateBr(continueLoop);

    context.currentLoopBlockContinue = oldLoopContinue;
    context.currentLoopBlockBreak = oldLoopBreak;

    // Finish by adding the break loop block and setting the insert point to it...
    context.currentFunction->getBasicBlockList().push_back(breakLoop);
    context.irBuilder.SetInsertPoint(breakLoop);
}

void gulc::CodeGen::generateDoStmt(gulc::CodeGenContext& context, const gulc::DoStmt* doStmt, const std::string& loopName) {
    std::string doName;

    if (loopName.empty()) {
        doName = "loop" + std::to_string(context.loopNameNumber);
        ++context.loopNameNumber;
    } else {
        doName = loopName;
    }

    llvm::BasicBlock* loop = llvm::BasicBlock::Create(context.llvmContext, doName + "_loop", context.currentFunction);
    llvm::BasicBlock* loopContinue = llvm::BasicBlock::Create(context.llvmContext, doName + "_continue");
    llvm::BasicBlock* loopBreak = llvm::BasicBlock::Create(context.llvmContext, doName + "_break");

    // For some reason we can't just fall through to the continue loop? We have to explicitly branch to it?
    context.irBuilder.CreateBr(loop);
    // Set the insert point to the loop block and start adding the loop data...
    context.irBuilder.SetInsertPoint(loop);

    // We make sure to back up and restore the old loop's break and continue blocks for our `break` and `continue` keywords
    llvm::BasicBlock* oldLoopContinue = context.currentLoopBlockContinue;
    llvm::BasicBlock* oldLoopBreak = context.currentLoopBlockBreak;

    context.currentLoopBlockContinue = loopContinue;
    context.currentLoopBlockBreak = loopBreak;

    // Generate the statement we loop on...
    context.enterNestedLoop(loopContinue, loopBreak);
    generateStmt(context, doStmt->loopStmt);
    context.leaveNestedLoop();
    context.irBuilder.CreateBr(loopContinue);

    context.currentLoopBlockContinue = oldLoopContinue;
    context.currentLoopBlockBreak = oldLoopBreak;

    // Add the loop continue block to the function and set it as the insert point...
    context.currentFunction->getBasicBlockList().push_back(loopContinue);
    context.irBuilder.SetInsertPoint(loopContinue);

    // Generate the condition and create the conditional branch
    llvm::Value* cond = generateExpr(context, doStmt->condition);
    context.irBuilder.CreateCondBr(cond, loop, loopBreak);

    // Add the loop break block to the function and set it as the insert point...
    context.currentFunction->getBasicBlockList().push_back(loopBreak);
    context.irBuilder.SetInsertPoint(loopBreak);
}

void gulc::CodeGen::generateForStmt(gulc::CodeGenContext& context, const gulc::ForStmt* forStmt, const std::string& loopName) {
    if (forStmt->preLoop != nullptr) {
        generateExpr(context, forStmt->preLoop);
    }

    std::string forName;

    if (loopName.empty()) {
        forName = "loop" + std::to_string(context.loopNameNumber);
        ++context.loopNameNumber;
    } else {
        forName = loopName;
    }

    llvm::BasicBlock* loop = llvm::BasicBlock::Create(context.llvmContext, forName + "_loop", context.currentFunction);
    llvm::BasicBlock* hiddenContinueLoop = llvm::BasicBlock::Create(context.llvmContext, forName + "_hidden_continue");
    llvm::BasicBlock* continueLoop = llvm::BasicBlock::Create(context.llvmContext, forName + "_continue");
    llvm::BasicBlock* breakLoop = llvm::BasicBlock::Create(context.llvmContext, forName + "_break");

    // Set the loop as the current insert point
    context.irBuilder.CreateBr(loop);
    context.irBuilder.SetInsertPoint(loop);

    llvm::Value* cond = generateExpr(context, forStmt->condition);
    // If the condition is true we continue the loop, if not we break from the loop...
    context.irBuilder.CreateCondBr(cond, hiddenContinueLoop, breakLoop);

    // Set the hidden continue loop as the current insert point
    context.currentFunction->getBasicBlockList().push_back(hiddenContinueLoop);
    context.irBuilder.SetInsertPoint(hiddenContinueLoop);

    // We make sure to back up and restore the old loop's break and continue blocks for our `break` and `continue` keywords
    llvm::BasicBlock* oldLoopContinue = context.currentLoopBlockContinue;
    llvm::BasicBlock* oldLoopBreak = context.currentLoopBlockBreak;

    context.currentLoopBlockContinue = continueLoop;
    context.currentLoopBlockBreak = breakLoop;

    // Generate the statement we loop on
    context.enterNestedLoop(continueLoop, breakLoop);
    generateStmt(context, forStmt->loopStmt);
    context.leaveNestedLoop();

    context.currentLoopBlockContinue = oldLoopContinue;
    context.currentLoopBlockBreak = oldLoopBreak;

    // Now we go to our actual continue block, the continue block has to be here so we apply the 'iterationExpr'
    context.irBuilder.CreateBr(continueLoop);
    context.currentFunction->getBasicBlockList().push_back(continueLoop);
    context.irBuilder.SetInsertPoint(continueLoop);

    // Generate the iteration expression (usually `++i`)
    generateExpr(context, forStmt->iterationExpr);

    // Branch back to the beginning of our loop...
    context.irBuilder.CreateBr(loop);

    // And then finish off by adding the break point.
    context.currentFunction->getBasicBlockList().push_back(breakLoop);
    context.irBuilder.SetInsertPoint(breakLoop);
}

void gulc::CodeGen::generateBreakStmt(gulc::CodeGenContext &context, const gulc::BreakStmt *breakStmt) {
    if (breakStmt->label().empty()) {
        context.irBuilder.CreateBr(context.currentLoopBlockBreak);
    } else {
        llvm::BasicBlock* breakBlock = context.getBreakBlock(breakStmt->label());

        if (breakBlock == nullptr) {
            printError("[INTERNAL] block '" + breakStmt->label() + "' not found!",
                       context.fileAst, breakStmt->startPosition(), breakStmt->endPosition());
            return;
        }

        context.irBuilder.CreateBr(breakBlock);
    }
}

void gulc::CodeGen::generateContinueStmt(gulc::CodeGenContext &context, const gulc::ContinueStmt *continueStmt) {
    if (continueStmt->label().empty()) {
        context.irBuilder.CreateBr(context.currentLoopBlockContinue);
    } else {
        llvm::BasicBlock* continueBlock = context.getContinueBlock(continueStmt->label());

        if (continueBlock == nullptr) {
            printError("[INTERNAL] block '" + continueStmt->label() + "' not found!",
                       context.fileAst, continueStmt->startPosition(), continueStmt->endPosition());
            return;
        }

        context.irBuilder.CreateBr(continueBlock);
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

    if (binaryOperatorExpr->operatorName() == "=") {
        return context.irBuilder.CreateStore(rightValue, leftValue, false);
    } else if (binaryOperatorExpr->operatorName() == "+") {
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
        context.addCurrentFunctionLabel(labeledStmt->label(), labelBody);
    }

    // We have to explicitly branch to blocks for some reason...
    context.irBuilder.CreateBr(labelBody);
    addBlockAndSetInsertionPoint(context, labelBody);

    if (labeledStmt->labeledStmt != nullptr) {
        generateStmt(context, labeledStmt->labeledStmt, labeledStmt->label());
    }
}

void gulc::CodeGen::generateGotoStmt(gulc::CodeGenContext &context, const gulc::GotoStmt *gotoStmt) {
    if (context.currentFunctionLabelsContains(gotoStmt->label())) {
        context.irBuilder.CreateBr(context.currentFunctionLabels[gotoStmt->label()]);
    } else {
        llvm::BasicBlock* newBasicBlock = llvm::BasicBlock::Create(context.llvmContext, gotoStmt->label());
        context.irBuilder.CreateBr(newBasicBlock);
        context.addCurrentFunctionLabel(gotoStmt->label(), newBasicBlock);
    }
}

llvm::Value *gulc::CodeGen::generateLocalVariableDeclExpr(gulc::CodeGenContext &context, const gulc::LocalVariableDeclExpr *localVariableDeclExpr) {
    return context.addLocalVariable(localVariableDeclExpr->name(), generateLlvmType(context, localVariableDeclExpr->resultType));
}

llvm::Value *gulc::CodeGen::generateIdentifierExpr(gulc::CodeGenContext &context, const gulc::IdentifierExpr *identifierExpr) {
    llvm::AllocaInst* localVariableAlloca = context.getLocalVariableOrNull(identifierExpr->name());

    if (localVariableAlloca != nullptr) {
        return localVariableAlloca;
    } else {
        printError("only local variables are currently supported!",
                   context.fileAst, identifierExpr->startPosition(), identifierExpr->endPosition());
    }

    return nullptr;
}

llvm::Value *gulc::CodeGen::generateLValueToRValue(gulc::CodeGenContext &context, const gulc::LValueToRValueExpr *lValueToRValueExpr) {
    llvm::Value* lValue = generateExpr(context, lValueToRValueExpr->lvalue);

    return context.irBuilder.CreateLoad(lValue, "l2r");
}

llvm::Value *gulc::CodeGen::generateFunctionCallExpr(gulc::CodeGenContext &context, const gulc::FunctionCallExpr *functionCallExpr) {
    if (!llvm::isa<IdentifierExpr>(functionCallExpr->functionReference)) {
        printError("function pointer and virtual member function calls not yet supported!",
                   context.fileAst, functionCallExpr->startPosition(), functionCallExpr->endPosition());
        return nullptr;
    }

    auto functionIdentifier = llvm::dyn_cast<IdentifierExpr>(functionCallExpr->functionReference);

    // NOTE: All error checking is should be handled before the code generator
    llvm::Function* func = context.module->getFunction(functionIdentifier->name());

    std::vector<llvm::Value*> llvmArgs{};

    if (functionCallExpr->hasArguments()) {
        llvmArgs.reserve(functionCallExpr->arguments.size());

        for (gulc::Expr *arg : functionCallExpr->arguments) {
            llvmArgs.push_back(generateExpr(context, arg));
        }
    }

    return context.irBuilder.CreateCall(func, llvmArgs, functionIdentifier->name() + "_result");
}

llvm::Value *gulc::CodeGen::generatePrefixOperatorExpr(gulc::CodeGenContext &context, const gulc::PrefixOperatorExpr *prefixOperatorExpr) {
    if (!llvm::isa<gulc::BuiltInType>(prefixOperatorExpr->resultType)) {
        printError("built in prefix operator called on non-built in type!",
                   context.fileAst, prefixOperatorExpr->startPosition(), prefixOperatorExpr->endPosition());
        return nullptr;
    }

    auto builtInType = llvm::dyn_cast<gulc::BuiltInType>(prefixOperatorExpr->resultType);

    llvm::Value* lvalue = generateExpr(context, prefixOperatorExpr->expr);
    llvm::Value* rvalue = context.irBuilder.CreateLoad(lvalue, "l2r");

    if (prefixOperatorExpr->operatorName() == "++") {
        if (builtInType->isFloating()) {
            llvm::Value* newValue = context.irBuilder.CreateFAdd(rvalue, llvm::ConstantFP::get(context.llvmContext, llvm::APFloat(1.0f)), "preinctmp");
            context.irBuilder.CreateStore(newValue, lvalue);
        } else {
            llvm::Value* newValue = context.irBuilder.CreateAdd(rvalue, llvm::ConstantInt::get(context.llvmContext, llvm::APInt(builtInType->size() * 8, 1)), "preinctmp");
            context.irBuilder.CreateStore(newValue, lvalue);
        }
    } else if (prefixOperatorExpr->operatorName() == "--") {
        if (builtInType->isFloating()) {
            llvm::Value* newValue = context.irBuilder.CreateFSub(rvalue, llvm::ConstantFP::get(context.llvmContext, llvm::APFloat(1.0f)), "predectmp");
            context.irBuilder.CreateStore(newValue, lvalue);
        } else {
            llvm::Value* newValue = context.irBuilder.CreateSub(rvalue, llvm::ConstantInt::get(context.llvmContext, llvm::APInt(builtInType->size() * 8, 1)), "predectmp");
            context.irBuilder.CreateStore(newValue, lvalue);
        }
    } else if (prefixOperatorExpr->operatorName() == "-") {
        if (builtInType->isFloating()) {
            return context.irBuilder.CreateFNeg(rvalue, "negtmp");
        } else {
            return context.irBuilder.CreateNeg(rvalue, "negtmp");
        }
    } else if (prefixOperatorExpr->operatorName() != "+") {
        printError("unknown built in prefix operator!",
                   context.fileAst, prefixOperatorExpr->startPosition(), prefixOperatorExpr->endPosition());
    }

    return lvalue;
}

llvm::Value *gulc::CodeGen::generatePostfixOperatorExpr(gulc::CodeGenContext &context, const gulc::PostfixOperatorExpr *postfixOperatorExpr) {
    if (!llvm::isa<gulc::BuiltInType>(postfixOperatorExpr->resultType)) {
        printError("built in postfix operator called on non-built in type!",
                   context.fileAst, postfixOperatorExpr->startPosition(), postfixOperatorExpr->endPosition());
        return nullptr;
    }

    auto builtInType = llvm::dyn_cast<gulc::BuiltInType>(postfixOperatorExpr->resultType);

    llvm::Value* lvalue = generateExpr(context, postfixOperatorExpr->expr);
    llvm::Value* rvalue = context.irBuilder.CreateLoad(lvalue, "l2r");

    if (postfixOperatorExpr->operatorName() == "++") {
        if (builtInType->isFloating()) {
            llvm::Value* newValue = context.irBuilder.CreateFAdd(rvalue, llvm::ConstantFP::get(context.llvmContext, llvm::APFloat(1.0f)), "preinctmp");
            context.irBuilder.CreateStore(newValue, lvalue);
        } else {
            llvm::Value* newValue = context.irBuilder.CreateAdd(rvalue, llvm::ConstantInt::get(context.llvmContext, llvm::APInt(builtInType->size() * 8, 1)), "preinctmp");
            context.irBuilder.CreateStore(newValue, lvalue);
        }
    } else if (postfixOperatorExpr->operatorName() == "--") {
        if (builtInType->isFloating()) {
            llvm::Value* newValue = context.irBuilder.CreateFSub(rvalue, llvm::ConstantFP::get(context.llvmContext, llvm::APFloat(1.0f)), "predectmp");
            context.irBuilder.CreateStore(newValue, lvalue);
        } else {
            llvm::Value* newValue = context.irBuilder.CreateSub(rvalue, llvm::ConstantInt::get(context.llvmContext, llvm::APInt(builtInType->size() * 8, 1)), "predectmp");
            context.irBuilder.CreateStore(newValue, lvalue);
        }
    } else {
        printError("unknown built in postfix operator!",
                   context.fileAst, postfixOperatorExpr->startPosition(), postfixOperatorExpr->endPosition());
    }

    return rvalue;
}
