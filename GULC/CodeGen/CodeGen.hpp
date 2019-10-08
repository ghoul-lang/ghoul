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

#ifndef GULC_CODEGEN_HPP
#define GULC_CODEGEN_HPP

#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LegacyPassManager.h>

#include <AST/FileAST.hpp>
#include <AST/Decls/FunctionDecl.hpp>
#include <AST/Exprs/BinaryOperatorExpr.hpp>
#include <AST/Exprs/IntegerLiteralExpr.hpp>
#include <AST/Exprs/FloatLiteralExpr.hpp>
#include <AST/Stmts/ReturnStmt.hpp>
#include <AST/Stmts/LabeledStmt.hpp>
#include <AST/Stmts/GotoStmt.hpp>
#include <AST/Exprs/LocalVariableDeclExpr.hpp>
#include <AST/Exprs/IdentifierExpr.hpp>
#include <AST/Exprs/LValueToRValueExpr.hpp>
#include <AST/Exprs/FunctionCallExpr.hpp>
#include <AST/Stmts/IfStmt.hpp>
#include <AST/Stmts/WhileStmt.hpp>
#include <AST/Stmts/DoStmt.hpp>
#include <AST/Stmts/ForStmt.hpp>
#include <AST/Exprs/PrefixOperatorExpr.hpp>
#include <AST/Exprs/PostfixOperatorExpr.hpp>
#include <AST/Stmts/BreakStmt.hpp>
#include <AST/Stmts/ContinueStmt.hpp>
#include <AST/Exprs/ImplicitCastExpr.hpp>
#include <AST/Exprs/RefFileFunctionExpr.hpp>
#include <AST/Exprs/RefLocalVariableExpr.hpp>
#include <AST/Exprs/RefParameterExpr.hpp>
#include <AST/Decls/GlobalVariableDecl.hpp>
#include <AST/Exprs/RefGlobalFileVariableExpr.hpp>
#include "Module.hpp"

namespace gulc {
    class CodeGen {
    public:
        CodeGen()
                : currentFileAst(nullptr), llvmContext(nullptr), irBuilder(nullptr), module(nullptr), funcPass(nullptr),
                  loopNameNumber(0),
                  currentFunction(nullptr), currentFunctionParameters(), entryBlockBuilder(nullptr),
                  currentFunctionLabels(), currentFunctionLocalVariablesCount(0),  currentFunctionLocalVariables(),
                  currentLoopBlockContinue(nullptr), currentLoopBlockBreak(nullptr),
                  nestedLoopCount(0), nestedLoopContinues(), nestedLoopBreaks() {}

        gulc::Module generate(FileAST& file);

    private:
        void printError(const std::string& message, TextPosition startPosition, TextPosition endPosition);

        llvm::GlobalObject* generateDecl(const Decl* decl);
        void generateStmt(const Stmt* stmt, const std::string& stmtName = "");
        llvm::Value* generateExpr(const Expr* expr);

        // Helpers
        void addBlockAndSetInsertionPoint(llvm::BasicBlock* basicBlock);

        // Decls
        llvm::Function* generateFunctionDecl(const FunctionDecl* functionDecl);
        llvm::GlobalVariable* generateGlobalVariableDecl(const GlobalVariableDecl* globalVariableDecl);

        // Stmts
        void generateCompoundStmt(const CompoundStmt* compoundStmt);
        void generateReturnStmt(const ReturnStmt* returnStmt);
        void generateLabeledStmt(const LabeledStmt* labeledStmt);
        void generateGotoStmt(const GotoStmt* gotoStmt);
        void generateIfStmt(const IfStmt* ifStmt);
        void generateWhileStmt(const WhileStmt* whileStmt, const std::string& loopName);
        void generateDoStmt(const DoStmt* doStmt, const std::string& loopName);
        void generateForStmt(const ForStmt* forStmt, const std::string& loopName);
        void generateBreakStmt(const BreakStmt* breakStmt);
        void generateContinueStmt(const ContinueStmt* continueStmt);

        // Types
        llvm::Type* generateLlvmType(const gulc::Type* type);
        std::vector<llvm::Type*> generateParamTypes(const std::vector<ParameterDecl*>& parameters);

        // Exprs
        llvm::Constant* generateConstant(const Expr* expr);

        llvm::Value* generateBinaryOperatorExpr(const BinaryOperatorExpr* binaryOperatorExpr);
        llvm::Value* generateIntegerLiteralExpr(const IntegerLiteralExpr* integerLiteralExpr);
        llvm::Value* generateFloatLiteralExpr(const FloatLiteralExpr* floatLiteralExpr);
        llvm::Value* generateLocalVariableDeclExpr(const LocalVariableDeclExpr* localVariableDeclExpr);
        llvm::Value* generateIdentifierExpr(const IdentifierExpr* identifierExpr);
        llvm::Value* generateImplicitCastExpr(const ImplicitCastExpr* implicitCastExpr);
        llvm::Value* generateLValueToRValue(const LValueToRValueExpr* lValueToRValueExpr);
        llvm::Value* generateFunctionCallExpr(const FunctionCallExpr* functionCallExpr);
        llvm::Value* generatePrefixOperatorExpr(const PrefixOperatorExpr* prefixOperatorExpr);
        llvm::Value* generatePostfixOperatorExpr(const PostfixOperatorExpr* postfixOperatorExpr);
        llvm::Value* generateRefLocalVariableExpr(const RefLocalVariableExpr* refLocalVariableExpr);
        llvm::Value* generateRefParameterExpr(const RefParameterExpr* refParameterExpr);
        llvm::Value* generateRefGlobalFileVariableExpr(const RefGlobalFileVariableExpr* refGlobalFileVariableExpr);

        llvm::Function* generateRefFunctionExpr(const Expr* expr, std::string* nameOut);
        llvm::Function* generateRefFileFunctionExpr(const RefFileFunctionExpr* refFileFunctionExpr);

        void castValue(gulc::Type* to, gulc::Type* from, llvm::Value*& value);

        // Context info
        FileAST* currentFileAst;
        llvm::LLVMContext* llvmContext;
        llvm::IRBuilder<>* irBuilder;
        llvm::Module* module;
        llvm::legacy::FunctionPassManager* funcPass;
        unsigned int loopNameNumber;

        llvm::Function* currentFunction;
        std::vector<llvm::AllocaInst*> currentFunctionParameters;
        llvm::IRBuilder<>* entryBlockBuilder;
        std::map<std::string, llvm::BasicBlock*> currentFunctionLabels;
        unsigned int currentFunctionLocalVariablesCount;
        std::vector<llvm::AllocaInst*> currentFunctionLocalVariables;

        llvm::BasicBlock* currentLoopBlockContinue;
        llvm::BasicBlock* currentLoopBlockBreak;

        unsigned int nestedLoopCount;
        std::vector<llvm::BasicBlock*> nestedLoopContinues;
        std::vector<llvm::BasicBlock*> nestedLoopBreaks;

        bool currentFunctionLabelsContains(const std::string& labelName) {
            return currentFunctionLabels.find(labelName) != currentFunctionLabels.end();
        }

        void addCurrentFunctionLabel(const std::string& labelName, llvm::BasicBlock* basicBlock) {
            if (currentFunctionLabels.find(labelName) == currentFunctionLabels.end()) {
                currentFunctionLabels.insert({labelName, basicBlock});
            }
        }

        void setCurrentFunction(llvm::Function* currentFunction) {
            if (entryBlockBuilder) {
                delete entryBlockBuilder;
                entryBlockBuilder = nullptr;
            }

            currentFunctionParameters.clear();
            currentFunctionLocalVariables.clear();
            currentFunctionLabels.clear();

            this->currentFunction = currentFunction;
            this->entryBlockBuilder = new llvm::IRBuilder<>(&currentFunction->getEntryBlock(),
                                                            currentFunction->getEntryBlock().begin());

            for (llvm::Argument& arg : currentFunction->args()) {
                llvm::AllocaInst* allocaInst = this->entryBlockBuilder->CreateAlloca(arg.getType(), nullptr);

                currentFunctionParameters.push_back(allocaInst);

                this->entryBlockBuilder->CreateStore(&arg, allocaInst);
            }
        }

        llvm::AllocaInst* addLocalVariable(const std::string& varName, llvm::Type* llvmType) {
            llvm::AllocaInst* allocaInst = this->entryBlockBuilder->CreateAlloca(llvmType, nullptr, varName);

            ++currentFunctionLocalVariablesCount;

            if (currentFunctionLocalVariablesCount >= currentFunctionLocalVariables.size()) {
                currentFunctionLocalVariables.push_back(allocaInst);
            } else {
                currentFunctionLocalVariables[currentFunctionLocalVariablesCount - 1] = allocaInst;
            }

            return allocaInst;
        }

        llvm::AllocaInst* getLocalVariableOrNull(const std::string& varName) {
            for (std::size_t i = 0; i < currentFunctionLocalVariablesCount; ++i) {
                if (currentFunctionLocalVariables[i]->getName() == varName) {
                    return currentFunctionLocalVariables[i];
                }
            }

            return nullptr;
        }

        void enterNestedLoop(llvm::BasicBlock* continueLoop, llvm::BasicBlock* breakLoop) {
            ++nestedLoopCount;

            if (nestedLoopCount >= nestedLoopContinues.size()) {
                nestedLoopContinues.push_back(continueLoop);
                nestedLoopBreaks.push_back(breakLoop);
            } else {
                nestedLoopContinues[nestedLoopCount - 1] = continueLoop;
                nestedLoopBreaks[nestedLoopCount - 1] = breakLoop;
            }
        }

        void leaveNestedLoop() {
            --nestedLoopCount;
        }

        llvm::BasicBlock* getBreakBlock(const std::string& blockName) {
            std::string breakBlockName = blockName + "_break";

            for (std::size_t i = 0; i < nestedLoopCount; ++i) {
                if (nestedLoopBreaks[i]->getName() == breakBlockName) {
                    return nestedLoopBreaks[i];
                }
            }

            return nullptr;
        }

        llvm::BasicBlock* getContinueBlock(const std::string& blockName) {
            std::string continueBlockName = blockName + "_continue";

            for (std::size_t i = 0; i < nestedLoopCount; ++i) {
                if (nestedLoopContinues[i]->getName() == continueBlockName) {
                    return nestedLoopContinues[i];
                }
            }

            return nullptr;
        }

    };
}

#endif //GULC_CODEGEN_HPP
