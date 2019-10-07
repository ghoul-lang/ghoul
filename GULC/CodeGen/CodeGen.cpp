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
#include "llvm/Transforms/InstCombine/InstCombine.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Transforms/Scalar/GVN.h"

#include <AST/Types/BuiltInType.hpp>
#include <AST/Types/PointerType.hpp>
#include <AST/Exprs/ParenExpr.hpp>
#include <AST/Types/ReferenceType.hpp>
#include <AST/Types/ConstType.hpp>
#include <AST/Types/ImmutType.hpp>
#include <AST/Types/MutType.hpp>
#include "CodeGen.hpp"

gulc::Module gulc::CodeGen::generate(gulc::FileAST& file) {
    llvm::LLVMContext* llvmContext = new llvm::LLVMContext();
    llvm::IRBuilder<> irBuilder(*llvmContext);
    // TODO: Should we remove the ending file extension?
    llvm::Module* genModule = new llvm::Module(file.filePath(), *llvmContext);
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

    currentFileAst = &file;
    this->llvmContext = llvmContext;
    this->irBuilder = &irBuilder;
    this->module = genModule;
    this->funcPass = funcPassManager;

    for (const gulc::Decl* decl : file.topLevelDecls()) {
        generateDecl(decl);

        //globalObject.print()
    }

    genModule->print(llvm::errs(), nullptr);

    funcPassManager->doFinalization();
    delete funcPassManager;

    return Module(file.filePath(), llvmContext, genModule);
}

void gulc::CodeGen::printError(const std::string &message, TextPosition startPosition, TextPosition endPosition) {
    std::cout << "gulc codegen error[" << currentFileAst->filePath() << ", "
                                    "{" << startPosition.line << ", " << startPosition.column << "} "
                                    "to {" << endPosition.line << ", " << endPosition.column << "}]: "
              << message
              << std::endl;
    std::exit(1);
}

llvm::Type *gulc::CodeGen::generateLlvmType(const gulc::Type* type) {
    switch (type->getTypeKind()) {
        case gulc::Type::Kind::BuiltIn: {
            auto builtInType = llvm::dyn_cast<gulc::BuiltInType>(type);

            // Void is special...
            if (builtInType->size() == 0) {
                return llvm::Type::getVoidTy(*llvmContext);
            }

            // Bool is special...
            if (builtInType->isBool()) {
                return llvm::Type::getInt1Ty(*llvmContext);
            }

            // Floats...
            if (builtInType->isFloating()) {
                switch (builtInType->size()) {
                    case 2:
                        return llvm::Type::getHalfTy(*llvmContext);
                    case 4:
                        return llvm::Type::getFloatTy(*llvmContext);
                    case 8:
                        return llvm::Type::getDoubleTy(*llvmContext);
                    //case 16: 128 bit is support on some platforms, we should support it...
                    default:
                        printError("unsupported floating point size for type '" + type->getString() + "'!",
                                   type->startPosition(), type->endPosition());
                        return nullptr;
                }
            }

            // Signed/unsigned ints...
            // Thing to not here is that LLVM handles the signedness of a variable by the operation you used, similar to assembly. That is why we don't check if it is signed here.
            switch (builtInType->size()) {
                case 1:
                    return llvm::Type::getInt8Ty(*llvmContext);
                case 2:
                    return llvm::Type::getInt16Ty(*llvmContext);
                case 4:
                    return llvm::Type::getInt32Ty(*llvmContext);
                case 8:
                    return llvm::Type::getInt64Ty(*llvmContext);
                    //case 16: 128 bit is supported on some platforms, we should support it...
                default:
                    printError("unsupported floating point size for type '" + type->getString() + "'!",
                               type->startPosition(), type->endPosition());
                    return nullptr;
            }
        }
        case gulc::Type::Kind::Pointer: {
            auto pointerType = llvm::dyn_cast<gulc::PointerType>(type);
            // TODO: Is this right? What is the address space stuff?
            return llvm::PointerType::getUnqual(generateLlvmType(pointerType->pointToType));
        }
        case gulc::Type::Kind::Reference: {
            auto referenceType = llvm::dyn_cast<gulc::ReferenceType>(type);
            return llvm::PointerType::getUnqual(generateLlvmType(referenceType->referenceToType));
        }
        case gulc::Type::Kind::Const: {
            auto constType = llvm::dyn_cast<ConstType>(type);
            return generateLlvmType(constType->pointToType);
        }
        case gulc::Type::Kind::Immut: {
            auto immutType = llvm::dyn_cast<ImmutType>(type);
            return generateLlvmType(immutType->pointToType);
        }
        case gulc::Type::Kind::Mut: {
            auto mutType = llvm::dyn_cast<MutType>(type);
            return generateLlvmType(mutType->pointToType);
        }
        default:
            printError("type '" + type->getString() + "' not yet supported!",
                       type->startPosition(), type->endPosition());
            return nullptr;
    }
    return nullptr;
}

std::vector<llvm::Type*> gulc::CodeGen::generateParamTypes(const std::vector<ParameterDecl*>& parameters) {
    std::vector<llvm::Type*> paramTypes{};
    paramTypes.reserve(parameters.size());

    for (const ParameterDecl* parameterDecl : parameters) {
        paramTypes.push_back(generateLlvmType(parameterDecl->type));
    }

    return paramTypes;
}

llvm::GlobalObject* gulc::CodeGen::generateDecl(const gulc::Decl *decl) {
    switch (decl->getDeclKind()) {
        case gulc::Decl::Kind::Function:
            return generateFunctionDecl(llvm::dyn_cast<gulc::FunctionDecl>(decl));
        default:
            printError("internal - unsupported decl!",
                       decl->startPosition(), decl->endPosition());
            break;
    }

	return nullptr;
}

void gulc::CodeGen::generateStmt(const gulc::Stmt *stmt, const std::string& stmtName) {
    switch (stmt->getStmtKind()) {
        case gulc::Stmt::Kind::Break:
            return generateBreakStmt(llvm::dyn_cast<BreakStmt>(stmt));
        case gulc::Stmt::Kind::Case:
            printError("`case` not yet supported!", stmt->startPosition(), stmt->endPosition());
            return;
        case gulc::Stmt::Kind::Compound:
            return generateCompoundStmt(llvm::dyn_cast<CompoundStmt>(stmt));
        case gulc::Stmt::Kind::Continue:
            return generateContinueStmt(llvm::dyn_cast<ContinueStmt>(stmt));
        case gulc::Stmt::Kind::Do:
            return generateDoStmt(llvm::dyn_cast<DoStmt>(stmt), stmtName);
        case gulc::Stmt::Kind::For:
            return generateForStmt(llvm::dyn_cast<ForStmt>(stmt), stmtName);
        case gulc::Stmt::Kind::Goto:
            return generateGotoStmt(llvm::dyn_cast<GotoStmt>(stmt));
        case gulc::Stmt::Kind::If:
            return generateIfStmt(llvm::dyn_cast<IfStmt>(stmt));
        case gulc::Stmt::Kind::Labeled:
            return generateLabeledStmt(llvm::dyn_cast<LabeledStmt>(stmt));
        case gulc::Stmt::Kind::Return:
            return generateReturnStmt(llvm::dyn_cast<ReturnStmt>(stmt));
        case gulc::Stmt::Kind::Switch:
            printError("`switch` not yet supported!", stmt->startPosition(), stmt->endPosition());
            return;
        case gulc::Stmt::Kind::Try:
            printError("`try` not yet supported!", stmt->startPosition(), stmt->endPosition());
            return;
        case gulc::Stmt::Kind::TryCatch:
            printError("`catch` not yet supported!", stmt->startPosition(), stmt->endPosition());
            return;
        case gulc::Stmt::Kind::TryFinally:
            printError("`finally` not yet supported!", stmt->startPosition(), stmt->endPosition());
            return;
        case gulc::Stmt::Kind::While:
            return generateWhileStmt(llvm::dyn_cast<WhileStmt>(stmt), stmtName);
        case gulc::Stmt::Kind::Expr:
            // TODO: Is this a memory leak? I can't figure out if we have to explicitly free the unused `Value*` or if LLVM is storing it somewhere else?
            //  I even went so far as to check the `clang` code generator and they explicitly state they're ignoring an `LValue` result...
            //  and when I go to the definition of `LValue` it has an `llvm::Value*` member that isn't freed in a constructor...
            generateExpr(llvm::dyn_cast<Expr>(stmt));
            break;
    }
}

llvm::Value* gulc::CodeGen::generateExpr(const Expr* expr) {
    switch (expr->getExprKind()) {
        case gulc::Expr::Kind::BinaryOperator:
            return generateBinaryOperatorExpr(llvm::dyn_cast<BinaryOperatorExpr>(expr));
        case gulc::Expr::Kind::CharacterLiteral:
            printError("character literals not yet supported!",
                       expr->startPosition(), expr->endPosition());
            return nullptr;
        case gulc::Expr::Kind::ExplicitCast:
            printError("explicit casts not yet supported!",
                       expr->startPosition(), expr->endPosition());
            return nullptr;
        case gulc::Expr::Kind::FloatLiteral:
            return generateFloatLiteralExpr(llvm::dyn_cast<FloatLiteralExpr>(expr));
        case gulc::Expr::Kind::FunctionCall:
            return generateFunctionCallExpr(llvm::dyn_cast<FunctionCallExpr>(expr));
        case gulc::Expr::Kind::Identifier:
            return generateIdentifierExpr(llvm::dyn_cast<IdentifierExpr>(expr));
        case gulc::Expr::Kind::ImplicitCast:
            return generateImplicitCastExpr(llvm::dyn_cast<ImplicitCastExpr>(expr));
        case gulc::Expr::Kind::IndexerCall:
            printError("indexer calls not yet supported!",
                       expr->startPosition(), expr->endPosition());
            return nullptr;
        case gulc::Expr::Kind::IntegerLiteral:
            return generateIntegerLiteralExpr(llvm::dyn_cast<IntegerLiteralExpr>(expr));
        case gulc::Expr::Kind::LocalVariableDecl:
            return generateLocalVariableDeclExpr(llvm::dyn_cast<LocalVariableDeclExpr>(expr));
        case gulc::Expr::Kind::MemberAccessCall:
            printError("member access calls not yet supported!",
                       expr->startPosition(), expr->endPosition());
            return nullptr;
        case gulc::Expr::Kind::Paren:
            return generateExpr(llvm::dyn_cast<ParenExpr>(expr)->containedExpr);
        case gulc::Expr::Kind::PostfixOperator:
            return generatePostfixOperatorExpr(llvm::dyn_cast<PostfixOperatorExpr>(expr));
        case gulc::Expr::Kind::PrefixOperator:
            return generatePrefixOperatorExpr(llvm::dyn_cast<PrefixOperatorExpr>(expr));
        case gulc::Expr::Kind::StringLiteral:
            printError("string literals not yet supported!",
                       expr->startPosition(), expr->endPosition());
            return nullptr;
        case gulc::Expr::Kind::Ternary:
            printError("ternary operators not yet supported!",
                       expr->startPosition(), expr->endPosition());
            return nullptr;
        case gulc::Expr::Kind::LValueToRValue:
            return generateLValueToRValue(llvm::dyn_cast<LValueToRValueExpr>(expr));
        case gulc::Expr::Kind::RefLocalVariable:
            return generateRefLocalVariableExpr(llvm::dyn_cast<RefLocalVariableExpr>(expr));
        case gulc::Expr::Kind::RefParameter:
            return generateRefParameterExpr(llvm::dyn_cast<RefParameterExpr>(expr));
        default:
            printError("unexpected expression type in code generator!",
                       expr->startPosition(), expr->endPosition());
            return nullptr;
    }
}

// Helpers
void gulc::CodeGen::addBlockAndSetInsertionPoint(llvm::BasicBlock* basicBlock) {
    currentFunction->getBasicBlockList().push_back(basicBlock);
    irBuilder->SetInsertPoint(basicBlock);
}

// Decls
llvm::Function* gulc::CodeGen::generateFunctionDecl(const gulc::FunctionDecl *functionDecl) {
    std::vector<llvm::Type*> paramTypes = generateParamTypes(functionDecl->parameters);
    llvm::Type* returnType = generateLlvmType(functionDecl->resultType);
    llvm::FunctionType* functionType = llvm::FunctionType::get(returnType, paramTypes, false);
    llvm::Function* function = llvm::Function::Create(functionType, llvm::Function::LinkageTypes::CommonLinkage, functionDecl->name(), module);

    llvm::BasicBlock* funcBody = llvm::BasicBlock::Create(*llvmContext, "entry", function);
    irBuilder->SetInsertPoint(funcBody);
    setCurrentFunction(function);

    // Generate the function body
    currentFunctionLocalVariablesCount = 0;
    generateStmt(functionDecl->body());
    currentFunctionLocalVariablesCount = 0;


    // TODO: CodeGen shouldn't have to handle this, this should be handled elsewhere (`CodeVerifier` should check that a function returns on all code paths)
    if (llvm::isa<BuiltInType>(functionDecl->resultType) &&
            llvm::dyn_cast<BuiltInType>(functionDecl->resultType)->size() == 0) {
        irBuilder->CreateRetVoid();
    }

    // TODO: We might want to remove this.
    verifyFunction(*function);
    funcPass->run(*function);

    // Reset the insertion point (this probably isn't needed but oh well)
    irBuilder->ClearInsertionPoint();
	currentFunction = nullptr;

    return function;
}

// Stmts
void gulc::CodeGen::generateCompoundStmt(const gulc::CompoundStmt* compoundStmt) {
    unsigned int oldLocalVariableCount = currentFunctionLocalVariablesCount;

    for (const Stmt* stmt : compoundStmt->statements()) {
        generateStmt(stmt);
    }

    currentFunctionLocalVariablesCount = oldLocalVariableCount;
}

void gulc::CodeGen::generateReturnStmt(const gulc::ReturnStmt *returnStmt) {
    if (returnStmt->hasReturnValue()) {
        irBuilder->CreateRet(generateExpr(returnStmt->returnValue));
        return;
    } else {
        irBuilder->CreateRetVoid();
        return;
    }
}

void gulc::CodeGen::generateLabeledStmt(const gulc::LabeledStmt *labeledStmt) {
    // Curious why we can't just use normal labels...
    llvm::BasicBlock* labelBody;

    if (currentFunctionLabelsContains(labeledStmt->label())) {
        labelBody = currentFunctionLabels[labeledStmt->label()];
    } else {
        labelBody = llvm::BasicBlock::Create(*llvmContext, labeledStmt->label());
        addCurrentFunctionLabel(labeledStmt->label(), labelBody);
    }

    // We have to explicitly branch to blocks for some reason...
    irBuilder->CreateBr(labelBody);
    addBlockAndSetInsertionPoint(labelBody);

    if (labeledStmt->labeledStmt != nullptr) {
        generateStmt(labeledStmt->labeledStmt, labeledStmt->label());
    }
}

void gulc::CodeGen::generateGotoStmt(const gulc::GotoStmt *gotoStmt) {
    if (currentFunctionLabelsContains(gotoStmt->label())) {
        irBuilder->CreateBr(currentFunctionLabels[gotoStmt->label()]);
    } else {
        llvm::BasicBlock* newBasicBlock = llvm::BasicBlock::Create(*llvmContext, gotoStmt->label());
        irBuilder->CreateBr(newBasicBlock);
        addCurrentFunctionLabel(gotoStmt->label(), newBasicBlock);
    }
}

void gulc::CodeGen::generateIfStmt(const gulc::IfStmt *ifStmt) {
    llvm::Value* cond = generateExpr(ifStmt->condition);

    llvm::BasicBlock* trueBlock = llvm::BasicBlock::Create(*llvmContext, "ifTrueBlock", currentFunction);
    llvm::BasicBlock* mergeBlock = llvm::BasicBlock::Create(*llvmContext, "ifMerge");
    llvm::BasicBlock* falseBlock = nullptr;

    // If there isn't a false block we make the IR jump to the merge block on false, else we make an actual false block
    if (ifStmt->hasFalseStmt()) {
        falseBlock = llvm::BasicBlock::Create(*llvmContext, "ifFalseBlock");

        irBuilder->CreateCondBr(cond, trueBlock, falseBlock);
    } else {
        irBuilder->CreateCondBr(cond, trueBlock, mergeBlock);
    }

    // Set the insert point to our true block then generate the statement for it...
    irBuilder->SetInsertPoint(trueBlock);
    generateStmt(ifStmt->trueStmt);
    irBuilder->CreateBr(mergeBlock);

    // And then add a jump to the merge block if there is a false statement
    if (ifStmt->hasFalseStmt()) {
        // Set the insert point to the false block and then generate the statement for it...
        currentFunction->getBasicBlockList().push_back(falseBlock);
        irBuilder->SetInsertPoint(falseBlock);
        generateStmt(ifStmt->falseStmt);
        // Branch to merge when done...
        irBuilder->CreateBr(mergeBlock);
    }

    // Add the merge block to the function and then set the insert point to it
    currentFunction->getBasicBlockList().push_back(mergeBlock);
    irBuilder->SetInsertPoint(mergeBlock);
}

void gulc::CodeGen::generateWhileStmt(const gulc::WhileStmt *whileStmt, const std::string& loopName) {
    std::string whileName;

    if (loopName.empty()) {
        whileName = "loop" + std::to_string(loopNameNumber);
        ++loopNameNumber;
    } else {
        whileName = loopName;
    }

    llvm::BasicBlock* continueLoop = llvm::BasicBlock::Create(*llvmContext, whileName + "_continue", currentFunction);
    llvm::BasicBlock* loop = llvm::BasicBlock::Create(*llvmContext, whileName + "_loop", currentFunction);
    llvm::BasicBlock* breakLoop = llvm::BasicBlock::Create(*llvmContext, whileName + "_break");

    // For some reason we can't just fall through to the continue loop? We have to explicitly branch to it?
    irBuilder->CreateBr(continueLoop);

    // Set the insert point to the continue loop block and start adding the loop data...
    irBuilder->SetInsertPoint(continueLoop);

    llvm::Value* cond = generateExpr(whileStmt->condition);
    // If the condition is true we continue the loop, if not we break from the loop...
    irBuilder->CreateCondBr(cond, loop, breakLoop);

    // Set the insert point to the loop block for our actual statement...
    irBuilder->SetInsertPoint(loop);

    // We make sure to back up and restore the old loop's break and continue blocks for our `break` and `continue` keywords
    llvm::BasicBlock* oldLoopContinue = currentLoopBlockContinue;
    llvm::BasicBlock* oldLoopBreak = currentLoopBlockBreak;

    currentLoopBlockContinue = continueLoop;
    currentLoopBlockBreak = breakLoop;

    // Generate the loop statement within the loop block then jump back to the continue block...
    enterNestedLoop(continueLoop, breakLoop);
    generateStmt(whileStmt->loopStmt);
    leaveNestedLoop();
    irBuilder->CreateBr(continueLoop);

    currentLoopBlockContinue = oldLoopContinue;
    currentLoopBlockBreak = oldLoopBreak;

    // Finish by adding the break loop block and setting the insert point to it...
    currentFunction->getBasicBlockList().push_back(breakLoop);
    irBuilder->SetInsertPoint(breakLoop);
}

void gulc::CodeGen::generateDoStmt(const gulc::DoStmt* doStmt, const std::string& loopName) {
    std::string doName;

    if (loopName.empty()) {
        doName = "loop" + std::to_string(loopNameNumber);
        ++loopNameNumber;
    } else {
        doName = loopName;
    }

    llvm::BasicBlock* loop = llvm::BasicBlock::Create(*llvmContext, doName + "_loop", currentFunction);
    llvm::BasicBlock* loopContinue = llvm::BasicBlock::Create(*llvmContext, doName + "_continue");
    llvm::BasicBlock* loopBreak = llvm::BasicBlock::Create(*llvmContext, doName + "_break");

    // For some reason we can't just fall through to the continue loop? We have to explicitly branch to it?
    irBuilder->CreateBr(loop);
    // Set the insert point to the loop block and start adding the loop data...
    irBuilder->SetInsertPoint(loop);

    // We make sure to back up and restore the old loop's break and continue blocks for our `break` and `continue` keywords
    llvm::BasicBlock* oldLoopContinue = currentLoopBlockContinue;
    llvm::BasicBlock* oldLoopBreak = currentLoopBlockBreak;

    currentLoopBlockContinue = loopContinue;
    currentLoopBlockBreak = loopBreak;

    // Generate the statement we loop on...
    enterNestedLoop(loopContinue, loopBreak);
    generateStmt(doStmt->loopStmt);
    leaveNestedLoop();
    irBuilder->CreateBr(loopContinue);

    currentLoopBlockContinue = oldLoopContinue;
    currentLoopBlockBreak = oldLoopBreak;

    // Add the loop continue block to the function and set it as the insert point...
    currentFunction->getBasicBlockList().push_back(loopContinue);
    irBuilder->SetInsertPoint(loopContinue);

    // Generate the condition and create the conditional branch
    llvm::Value* cond = generateExpr(doStmt->condition);
    irBuilder->CreateCondBr(cond, loop, loopBreak);

    // Add the loop break block to the function and set it as the insert point...
    currentFunction->getBasicBlockList().push_back(loopBreak);
    irBuilder->SetInsertPoint(loopBreak);
}

void gulc::CodeGen::generateForStmt(const gulc::ForStmt* forStmt, const std::string& loopName) {
    if (forStmt->preLoop != nullptr) {
        generateExpr(forStmt->preLoop);
    }

    std::string forName;

    if (loopName.empty()) {
        forName = "loop" + std::to_string(loopNameNumber);
        ++loopNameNumber;
    } else {
        forName = loopName;
    }

    llvm::BasicBlock* loop = llvm::BasicBlock::Create(*llvmContext, forName + "_loop", currentFunction);
    llvm::BasicBlock* hiddenContinueLoop = llvm::BasicBlock::Create(*llvmContext, forName + "_hidden_continue");
    llvm::BasicBlock* continueLoop = llvm::BasicBlock::Create(*llvmContext, forName + "_continue");
    llvm::BasicBlock* breakLoop = llvm::BasicBlock::Create(*llvmContext, forName + "_break");

    // Set the loop as the current insert point
    irBuilder->CreateBr(loop);
    irBuilder->SetInsertPoint(loop);

    llvm::Value* cond = generateExpr(forStmt->condition);
    // If the condition is true we continue the loop, if not we break from the loop...
    irBuilder->CreateCondBr(cond, hiddenContinueLoop, breakLoop);

    // Set the hidden continue loop as the current insert point
    currentFunction->getBasicBlockList().push_back(hiddenContinueLoop);
    irBuilder->SetInsertPoint(hiddenContinueLoop);

    // We make sure to back up and restore the old loop's break and continue blocks for our `break` and `continue` keywords
    llvm::BasicBlock* oldLoopContinue = currentLoopBlockContinue;
    llvm::BasicBlock* oldLoopBreak = currentLoopBlockBreak;

    currentLoopBlockContinue = continueLoop;
    currentLoopBlockBreak = breakLoop;

    // Generate the statement we loop on
    enterNestedLoop(continueLoop, breakLoop);
    generateStmt(forStmt->loopStmt);
    leaveNestedLoop();

    currentLoopBlockContinue = oldLoopContinue;
    currentLoopBlockBreak = oldLoopBreak;

    // Now we go to our actual continue block, the continue block has to be here so we apply the 'iterationExpr'
    irBuilder->CreateBr(continueLoop);
    currentFunction->getBasicBlockList().push_back(continueLoop);
    irBuilder->SetInsertPoint(continueLoop);

    // Generate the iteration expression (usually `++i`)
    generateExpr(forStmt->iterationExpr);

    // Branch back to the beginning of our loop...
    irBuilder->CreateBr(loop);

    // And then finish off by adding the break point.
    currentFunction->getBasicBlockList().push_back(breakLoop);
    irBuilder->SetInsertPoint(breakLoop);
}

void gulc::CodeGen::generateBreakStmt(const gulc::BreakStmt *breakStmt) {
    if (breakStmt->label().empty()) {
        irBuilder->CreateBr(currentLoopBlockBreak);
    } else {
        llvm::BasicBlock* breakBlock = getBreakBlock(breakStmt->label());

        if (breakBlock == nullptr) {
            printError("[INTERNAL] block '" + breakStmt->label() + "' not found!",
                       breakStmt->startPosition(), breakStmt->endPosition());
            return;
        }

        irBuilder->CreateBr(breakBlock);
    }
}

void gulc::CodeGen::generateContinueStmt(const gulc::ContinueStmt *continueStmt) {
    if (continueStmt->label().empty()) {
        irBuilder->CreateBr(currentLoopBlockContinue);
    } else {
        llvm::BasicBlock* continueBlock = getContinueBlock(continueStmt->label());

        if (continueBlock == nullptr) {
            printError("[INTERNAL] block '" + continueStmt->label() + "' not found!",
                       continueStmt->startPosition(), continueStmt->endPosition());
            return;
        }

        irBuilder->CreateBr(continueBlock);
    }
}

// Exprs
llvm::Value* gulc::CodeGen::generateBinaryOperatorExpr(const gulc::BinaryOperatorExpr *binaryOperatorExpr) {
    llvm::Value* leftValue = generateExpr(binaryOperatorExpr->leftValue);
    llvm::Value* rightValue = generateExpr(binaryOperatorExpr->rightValue);

    gulc::Type* resultType = binaryOperatorExpr->resultType;

    // Ignore the const, mut, and immut...
    if (llvm::isa<ConstType>(resultType)) {
        resultType = llvm::dyn_cast<ConstType>(resultType)->pointToType;
    } else if (llvm::isa<ImmutType>(resultType)) {
        resultType = llvm::dyn_cast<ImmutType>(resultType)->pointToType;
    } else if (llvm::isa<MutType>(resultType)) {
        resultType = llvm::dyn_cast<MutType>(resultType)->pointToType;
    }

    // TODO: Support the `PointerType`
    bool isFloat = false;
    bool isSigned = false;

    if (llvm::isa<BuiltInType>(resultType)) {
        auto builtInType = llvm::dyn_cast<BuiltInType>(resultType);

        isFloat = builtInType->isFloating();
        isSigned = builtInType->isSigned();
    } else if (!(llvm::isa<PointerType>(resultType) ||
                 llvm::isa<ReferenceType>(resultType))) {
        printError("unknown binary operator expression!",
                   binaryOperatorExpr->startPosition(), binaryOperatorExpr->endPosition());
        return nullptr;
    }

    if (binaryOperatorExpr->operatorName() == "=") {
        return irBuilder->CreateStore(rightValue, leftValue, false);
    } else if (binaryOperatorExpr->operatorName() == "+") {
        if (isFloat) {
            return irBuilder->CreateFAdd(leftValue, rightValue, "addtmp");
        } else {
            return irBuilder->CreateAdd(leftValue, rightValue, "addtmp");
        }
    } else if (binaryOperatorExpr->operatorName() == "-") {
        if (isFloat) {
            return irBuilder->CreateFSub(leftValue, rightValue, "subtmp");
        } else {
            return irBuilder->CreateSub(leftValue, rightValue, "subtmp");
        }
    } else if (binaryOperatorExpr->operatorName() == "*") {
        if (isFloat) {
            return irBuilder->CreateFMul(leftValue, rightValue, "multmp");
        } else {
            return irBuilder->CreateMul(leftValue, rightValue, "multmp");
        }
    } else if (binaryOperatorExpr->operatorName() == "/") {
        if (isFloat) {
            return irBuilder->CreateFDiv(leftValue, rightValue, "divtmp");
        } else {
            // TODO: What are the `Exact` variants?
            if (isSigned) {
                return irBuilder->CreateSDiv(leftValue, rightValue, "divtmp");
            } else {
                return irBuilder->CreateUDiv(leftValue, rightValue, "divtmp");
            }
        }
    } else if (binaryOperatorExpr->operatorName() == "%") {
        if (isFloat) {
            return irBuilder->CreateFRem(leftValue, rightValue, "remtmp");
        } else {
            // TODO: What are the `Exact` variants?
            if (isSigned) {
                return irBuilder->CreateSRem(leftValue, rightValue, "remtmp");
            } else {
                return irBuilder->CreateURem(leftValue, rightValue, "remtmp");
            }
        }
    } else if (binaryOperatorExpr->operatorName() == "^") {
        return irBuilder->CreateXor(leftValue, rightValue, "xortmp");
    } else if (binaryOperatorExpr->operatorName() == "<<") {
        return irBuilder->CreateShl(leftValue, rightValue, "shltmp");
    } else if (binaryOperatorExpr->operatorName() == ">>") {
        if (isSigned) {
            return irBuilder->CreateAShr(leftValue, rightValue, "ashrtmp");
        } else {
            return irBuilder->CreateLShr(leftValue, rightValue, "lshrtmp");
        }
    } else if (binaryOperatorExpr->operatorName() == "==") {
        if (isFloat) {
            return irBuilder->CreateFCmpOEQ(leftValue, rightValue, "eqtmp");
        } else {
            return irBuilder->CreateICmpEQ(leftValue, rightValue, "eqtmp");
        }
    } else if (binaryOperatorExpr->operatorName() == ">") {
        if (isFloat) {
            return irBuilder->CreateFCmpOGT(leftValue, rightValue, "gttmp");
        } else {
            if (isSigned) {
                return irBuilder->CreateICmpSGT(leftValue, rightValue, "gttmp");
            } else {
                return irBuilder->CreateICmpUGT(leftValue, rightValue, "gttmp");
            }
        }
    } else if (binaryOperatorExpr->operatorName() == ">=") {
        if (isFloat) {
            return irBuilder->CreateFCmpOGE(leftValue, rightValue, "getmp");
        } else {
            if (isSigned) {
                return irBuilder->CreateICmpSGE(leftValue, rightValue, "getmp");
            } else {
                return irBuilder->CreateICmpUGE(leftValue, rightValue, "getmp");
            }
        }
    } else if (binaryOperatorExpr->operatorName() == "<") {
        if (isFloat) {
            return irBuilder->CreateFCmpOLT(leftValue, rightValue, "lttmp");
        } else {
            if (isSigned) {
                return irBuilder->CreateICmpSLT(leftValue, rightValue, "lttmp");
            } else {
                return irBuilder->CreateICmpULT(leftValue, rightValue, "lttmp");
            }
        }
    } else if (binaryOperatorExpr->operatorName() == "<=") {
        if (isFloat) {
            return irBuilder->CreateFCmpOLE(leftValue, rightValue, "letmp");
        } else {
            if (isSigned) {
                return irBuilder->CreateICmpSLE(leftValue, rightValue, "letmp");
            } else {
                return irBuilder->CreateICmpULE(leftValue, rightValue, "letmp");
            }
        }
    } else {
        printError("binary operator '" + binaryOperatorExpr->operatorName() + "' not yet supported!",
                   binaryOperatorExpr->startPosition(), binaryOperatorExpr->endPosition());
        return nullptr;
    }
}

llvm::Value *gulc::CodeGen::generateIntegerLiteralExpr(const gulc::IntegerLiteralExpr *integerLiteralExpr) {
    if (auto builtInType = llvm::dyn_cast<BuiltInType>(integerLiteralExpr->resultType)) {
        unsigned int numOfBits = builtInType->size() * 8;
        return llvm::ConstantInt::get(*llvmContext, llvm::APInt(numOfBits, integerLiteralExpr->numberString(), integerLiteralExpr->numberBase()));
    } else {
        printError("unknown integer literal type!",
                   integerLiteralExpr->startPosition(), integerLiteralExpr->endPosition());
        return nullptr;
    }
}

llvm::Value *gulc::CodeGen::generateFloatLiteralExpr(const gulc::FloatLiteralExpr *floatLiteralExpr) {
    if (auto builtInType = llvm::dyn_cast<BuiltInType>(floatLiteralExpr->resultType)) {
        switch (builtInType->size()) {
            case 2:
                return llvm::ConstantFP::get(*llvmContext, llvm::APFloat(llvm::APFloat::IEEEhalf(), floatLiteralExpr->numberValue()));
            case 4:
                return llvm::ConstantFP::get(*llvmContext, llvm::APFloat(llvm::APFloat::IEEEsingle(), floatLiteralExpr->numberValue()));
            case 8:
                return llvm::ConstantFP::get(*llvmContext, llvm::APFloat(llvm::APFloat::IEEEdouble(), floatLiteralExpr->numberValue()));
            // case 16: // TODO: Support the quad
            default:
                printError("unsupported floating point size!",
                           floatLiteralExpr->startPosition(), floatLiteralExpr->endPosition());
                return nullptr;
        }
    }

    printError("unknown float literal type!",
               floatLiteralExpr->startPosition(), floatLiteralExpr->endPosition());
    return nullptr;
}

llvm::Value *gulc::CodeGen::generateLocalVariableDeclExpr(const gulc::LocalVariableDeclExpr *localVariableDeclExpr) {
    return addLocalVariable(localVariableDeclExpr->name(), generateLlvmType(localVariableDeclExpr->resultType));
}

llvm::Value *gulc::CodeGen::generateIdentifierExpr(const gulc::IdentifierExpr *identifierExpr) {
    printError("[INTERNAL] identifier found in codegen, expression not supported!",
               identifierExpr->startPosition(), identifierExpr->endPosition());
    return nullptr;
}

llvm::Value *gulc::CodeGen::generateImplicitCastExpr(const gulc::ImplicitCastExpr *implicitCastExpr) {
    llvm::Value* result = generateExpr(implicitCastExpr->castee);
    castValue(implicitCastExpr->castType, implicitCastExpr->castee->resultType, result);
    return result;
}

llvm::Value *gulc::CodeGen::generateLValueToRValue(const gulc::LValueToRValueExpr *lValueToRValueExpr) {
    llvm::Value* lValue = generateExpr(lValueToRValueExpr->lvalue);

    return irBuilder->CreateLoad(lValue, "l2r");
}

llvm::Value *gulc::CodeGen::generateFunctionCallExpr(const gulc::FunctionCallExpr *functionCallExpr) {
    std::string funcName;
    llvm::Function* func = generateRefFunctionExpr(functionCallExpr->functionReference, &funcName);

    std::vector<llvm::Value*> llvmArgs{};

    if (functionCallExpr->hasArguments()) {
        llvmArgs.reserve(functionCallExpr->arguments.size());

        for (gulc::Expr *arg : functionCallExpr->arguments) {
            llvmArgs.push_back(generateExpr(arg));
        }
    }

    return irBuilder->CreateCall(func, llvmArgs, funcName + "_result");
}

llvm::Value *gulc::CodeGen::generatePrefixOperatorExpr(const gulc::PrefixOperatorExpr *prefixOperatorExpr) {
    gulc::Type* exprResultType = prefixOperatorExpr->expr->resultType;

    // Ignore the const, mut, and immut...
    if (llvm::isa<ConstType>(exprResultType)) {
        exprResultType = llvm::dyn_cast<ConstType>(exprResultType)->pointToType;
    } else if (llvm::isa<ImmutType>(exprResultType)) {
        exprResultType = llvm::dyn_cast<ImmutType>(exprResultType)->pointToType;
    } else if (llvm::isa<MutType>(exprResultType)) {
        exprResultType = llvm::dyn_cast<MutType>(exprResultType)->pointToType;
    }

    if (llvm::isa<gulc::BuiltInType>(exprResultType)) {
        auto builtInType = llvm::dyn_cast<gulc::BuiltInType>(exprResultType);

        llvm::Value* lvalue = generateExpr(prefixOperatorExpr->expr);
        llvm::Value* rvalue = irBuilder->CreateLoad(lvalue, "l2r");

        if (prefixOperatorExpr->operatorName() == "++") {
            if (builtInType->isFloating()) {
                llvm::Value* newValue = irBuilder->CreateFAdd(rvalue, llvm::ConstantFP::get(*llvmContext, llvm::APFloat(1.0f)), "preinctmp");
                irBuilder->CreateStore(newValue, lvalue);
            } else {
                llvm::Value* newValue = irBuilder->CreateAdd(rvalue, llvm::ConstantInt::get(*llvmContext, llvm::APInt(builtInType->size() * 8, 1)), "preinctmp");
                irBuilder->CreateStore(newValue, lvalue);
            }
        } else if (prefixOperatorExpr->operatorName() == "--") {
            if (builtInType->isFloating()) {
                llvm::Value* newValue = irBuilder->CreateFSub(rvalue, llvm::ConstantFP::get(*llvmContext, llvm::APFloat(1.0f)), "predectmp");
                irBuilder->CreateStore(newValue, lvalue);
            } else {
                llvm::Value* newValue = irBuilder->CreateSub(rvalue, llvm::ConstantInt::get(*llvmContext, llvm::APInt(builtInType->size() * 8, 1)), "predectmp");
                irBuilder->CreateStore(newValue, lvalue);
            }
        } else if (prefixOperatorExpr->operatorName() == "-") {
            if (builtInType->isFloating()) {
                return irBuilder->CreateFNeg(rvalue, "negtmp");
            } else {
                return irBuilder->CreateNeg(rvalue, "negtmp");
            }
        } else if (prefixOperatorExpr->operatorName() == "&" || prefixOperatorExpr->operatorName() == ".ref") {
            // TODO: Should we try to do error checking here?
            return lvalue;
        } else if (prefixOperatorExpr->operatorName() != "+") {
            printError("unknown built in prefix operator '" + prefixOperatorExpr->operatorName() +  "'!",
                       prefixOperatorExpr->startPosition(), prefixOperatorExpr->endPosition());
        }

        return lvalue;
    } else if (llvm::isa<PointerType>(exprResultType)) {
        llvm::Value *lvalue = generateExpr(prefixOperatorExpr->expr);

        if (prefixOperatorExpr->operatorName() == "*") {
            return irBuilder->CreateLoad(lvalue, "deref");
        } else if (prefixOperatorExpr->operatorName() == "++" || prefixOperatorExpr->operatorName() == "--") {
            // TODO: We need to know the size of a pointer to support this...
            printError("increment and decrement operators not yet supported on pointer types!",
                       prefixOperatorExpr->startPosition(), prefixOperatorExpr->endPosition());
        } else {
            printError("unknown prefix operator '" + prefixOperatorExpr->operatorName() + "' used on pointer type!",
                       prefixOperatorExpr->startPosition(), prefixOperatorExpr->endPosition());
        }
    } else if (llvm::isa<ReferenceType>(exprResultType)) {
        llvm::Value *lvalue = generateExpr(prefixOperatorExpr->expr);
        llvm::Value *rvalue = irBuilder->CreateLoad(lvalue, "l2r");

        if (prefixOperatorExpr->operatorName() == ".deref") {
            return irBuilder->CreateLoad(rvalue, "deref");
        } else if (prefixOperatorExpr->operatorName() == "++" || prefixOperatorExpr->operatorName() == "--") {
            // TODO: We need to know the size of a pointer to support this...
            printError("increment and decrement operators not yet supported on reference types!",
                       prefixOperatorExpr->startPosition(), prefixOperatorExpr->endPosition());
        } else {
            printError("unknown prefix operator '" + prefixOperatorExpr->operatorName() + "' used on reference type!",
                       prefixOperatorExpr->startPosition(), prefixOperatorExpr->endPosition());
        }
    } else {
        printError("built in prefix operator called on unsupported type!",
                   prefixOperatorExpr->startPosition(), prefixOperatorExpr->endPosition());
    }

    return nullptr;
}

llvm::Value *gulc::CodeGen::generatePostfixOperatorExpr(const gulc::PostfixOperatorExpr *postfixOperatorExpr) {
    if (!llvm::isa<gulc::BuiltInType>(postfixOperatorExpr->resultType)) {
        printError("built in postfix operator called on non-built in type!",
                   postfixOperatorExpr->startPosition(), postfixOperatorExpr->endPosition());
        return nullptr;
    }

    auto builtInType = llvm::dyn_cast<gulc::BuiltInType>(postfixOperatorExpr->resultType);

    llvm::Value* lvalue = generateExpr(postfixOperatorExpr->expr);
    llvm::Value* rvalue = irBuilder->CreateLoad(lvalue, "l2r");

    if (postfixOperatorExpr->operatorName() == "++") {
        if (builtInType->isFloating()) {
            llvm::Value* newValue = irBuilder->CreateFAdd(rvalue, llvm::ConstantFP::get(*llvmContext, llvm::APFloat(1.0f)), "preinctmp");
            irBuilder->CreateStore(newValue, lvalue);
        } else {
            llvm::Value* newValue = irBuilder->CreateAdd(rvalue, llvm::ConstantInt::get(*llvmContext, llvm::APInt(builtInType->size() * 8, 1)), "preinctmp");
            irBuilder->CreateStore(newValue, lvalue);
        }
    } else if (postfixOperatorExpr->operatorName() == "--") {
        if (builtInType->isFloating()) {
            llvm::Value* newValue = irBuilder->CreateFSub(rvalue, llvm::ConstantFP::get(*llvmContext, llvm::APFloat(1.0f)), "predectmp");
            irBuilder->CreateStore(newValue, lvalue);
        } else {
            llvm::Value* newValue = irBuilder->CreateSub(rvalue, llvm::ConstantInt::get(*llvmContext, llvm::APInt(builtInType->size() * 8, 1)), "predectmp");
            irBuilder->CreateStore(newValue, lvalue);
        }
    } else {
        printError("unknown built in postfix operator!",
                   postfixOperatorExpr->startPosition(), postfixOperatorExpr->endPosition());
    }

    return rvalue;
}

llvm::Value *gulc::CodeGen::generateRefLocalVariableExpr(const gulc::RefLocalVariableExpr *refLocalVariableExpr) {
    llvm::AllocaInst* localVariableAlloca = getLocalVariableOrNull(refLocalVariableExpr->name());

    if (localVariableAlloca != nullptr) {
        return localVariableAlloca;
    } else {
        printError("[INTERNAL] local variable was not found!",
                   refLocalVariableExpr->startPosition(), refLocalVariableExpr->endPosition());
    }

    return nullptr;
}

llvm::Value *gulc::CodeGen::generateRefParameterExpr(const gulc::RefParameterExpr *refParameterExpr) {
    if (refParameterExpr->paramIndex() < currentFunctionParameters.size()) {
        return currentFunctionParameters[refParameterExpr->paramIndex()];
    }

    printError("[INTERNAL] parameter was not found!",
               refParameterExpr->startPosition(), refParameterExpr->endPosition());
    return nullptr;
}

llvm::Function *gulc::CodeGen::generateRefFunctionExpr(const gulc::Expr *expr, std::string *nameOut) {
    switch (expr->getExprKind()) {
        case gulc::Expr::Kind::RefFileFunction: {
            auto refFileFunction = llvm::dyn_cast<RefFileFunctionExpr>(expr);
            *nameOut = refFileFunction->name();
            return generateRefFileFunctionExpr(refFileFunction);
        }
        default:
            printError("[INTERNAL] unsupported function reference!",
                       expr->startPosition(), expr->endPosition());
            return nullptr;
    }
}

llvm::Function *gulc::CodeGen::generateRefFileFunctionExpr(const gulc::RefFileFunctionExpr *refFileFunctionExpr) {
    // NOTE: All error checking is should be handled before the code generator
    return module->getFunction(refFileFunctionExpr->name());
}

void gulc::CodeGen::castValue(gulc::Type *to, gulc::Type *from, llvm::Value*& value) {
    // We ignore const, mut, and immut here since LLVM doesn't have any concept of these...
    if (llvm::isa<ConstType>(to)) {
        to = llvm::dyn_cast<ConstType>(to)->pointToType;
    } else if (llvm::isa<MutType>(to)) {
        to = llvm::dyn_cast<MutType>(to)->pointToType;
    } else if (llvm::isa<ImmutType>(to)) {
        to = llvm::dyn_cast<ImmutType>(to)->pointToType;
    }

    if (llvm::isa<ConstType>(from)) {
        from = llvm::dyn_cast<ConstType>(from)->pointToType;
    } else if (llvm::isa<MutType>(from)) {
        from = llvm::dyn_cast<MutType>(from)->pointToType;
    } else if (llvm::isa<ImmutType>(from)) {
        from = llvm::dyn_cast<ImmutType>(from)->pointToType;
    }

    if (llvm::isa<BuiltInType>(from)) {
        auto fromBuiltIn = llvm::dyn_cast<BuiltInType>(from);

        if (llvm::isa<BuiltInType>(to)) {
            auto toBuiltIn = llvm::dyn_cast<BuiltInType>(to);

            if (fromBuiltIn->isFloating()) {
                if (toBuiltIn->isFloating()) {
                    if (toBuiltIn->size() > fromBuiltIn->size()) {
                        value = irBuilder->CreateFPExt(value, generateLlvmType(to), "fpext");
                        return;
                    } else {
                        value = irBuilder->CreateFPTrunc(value, generateLlvmType(to), "fptrunc");
                        return;
                    }
                } else if (toBuiltIn->isSigned()) {
                    value = irBuilder->CreateFPToSI(value, generateLlvmType(to), "fp2si");
                    return;
                } else {
                    value = irBuilder->CreateFPToUI(value, generateLlvmType(to), "fp2ui");
                    return;
                }
            } else if (fromBuiltIn->isSigned()) {
                if (toBuiltIn->isFloating()) {
                    value = irBuilder->CreateSIToFP(value, generateLlvmType(to), "si2fp");
                    return;
                } else {
                    // TODO: I'm not sure if the `isSigned` is meant for `value` or `destTy`? Assuming value...
                    value = irBuilder->CreateIntCast(value, generateLlvmType(to), fromBuiltIn->isSigned(), "si2int");
                    return;
                }
            } else {
                if (toBuiltIn->isFloating()) {
                    value = irBuilder->CreateUIToFP(value, generateLlvmType(to), "ui2fp");
                    return;
                } else {
                    // TODO: I'm not sure if the `isSigned` is meant for `value` or `destTy`? Assuming value...
                    value = irBuilder->CreateIntCast(value, generateLlvmType(to), fromBuiltIn->isSigned(), "ui2int");
                    return;
                }
            }
        } else if (llvm::isa<PointerType>(to)) {
            if (fromBuiltIn->isFloating()) {
                printError("[INTERNAL] casting from a float to a pointer is NOT supported!",
                           to->startPosition(), to->endPosition());
                return;
            }

            value = irBuilder->CreateIntToPtr(value, generateLlvmType(to), "int2ptr");
            return;
        }
    }

    // TODO: We need to have the start and end positions passed to us
    printError("casting to type `" + to->getString() + "` from type `" + from->getString() + "` is not supported!",
               to->startPosition(), to->endPosition());
}
