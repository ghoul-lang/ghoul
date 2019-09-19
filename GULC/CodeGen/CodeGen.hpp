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
#include "Module.hpp"

namespace gulc {
    // TODO: This doesn't really need to exist. Not sure why I've been doing it this way. We can really just make all of this apart of `CodeGen`
    struct CodeGenContext {
        FileAST& fileAst;
        llvm::LLVMContext& llvmContext;
        llvm::IRBuilder<>& irBuilder;
        llvm::Module* module;
        llvm::legacy::FunctionPassManager* funcPass;
        unsigned int loopNameNumber;

        llvm::Function* currentFunction;
        llvm::IRBuilder<>* entryBlockBuilder;
        std::map<std::string, llvm::BasicBlock*> currentFunctionLabels;
        unsigned int currentFunctionLocalVariablesCount;
        std::vector<llvm::AllocaInst*> currentFunctionLocalVariables;

        llvm::BasicBlock* currentLoopBlockContinue;
        llvm::BasicBlock* currentLoopBlockBreak;

        unsigned int nestedLoopCount;
        std::vector<llvm::BasicBlock*> nestedLoopContinues;
        std::vector<llvm::BasicBlock*> nestedLoopBreaks;

        CodeGenContext(FileAST& fileAst, llvm::LLVMContext& llvmContext, llvm::IRBuilder<>& irBuilder,
                       llvm::Module* module, llvm::legacy::FunctionPassManager* funcPass)
                : fileAst(fileAst), llvmContext(llvmContext), irBuilder(irBuilder), module(module), funcPass(funcPass),
                  loopNameNumber(0),
                  currentFunction(nullptr), entryBlockBuilder(nullptr), currentFunctionLabels(),
				  currentFunctionLocalVariablesCount(0),  currentFunctionLocalVariables(),
                  currentLoopBlockContinue(nullptr), currentLoopBlockBreak(nullptr),
                  nestedLoopCount(0), nestedLoopContinues(), nestedLoopBreaks() {}

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

            currentFunctionLocalVariables.clear();
            currentFunctionLabels.clear();

            this->currentFunction = currentFunction;
            this->entryBlockBuilder = new llvm::IRBuilder<>(&currentFunction->getEntryBlock(),
                                                            currentFunction->getEntryBlock().begin());
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

    class CodeGen {
    public:
        gulc::Module generate(FileAST& file);

    private:
        void printError(const std::string& message, FileAST &fileAst, TextPosition startPosition, TextPosition endPosition);

        llvm::GlobalObject* generateDecl(CodeGenContext& context, const Decl* decl);
        void generateStmt(CodeGenContext& context, const Stmt* stmt, const std::string& stmtName = "");
        llvm::Value* generateExpr(CodeGenContext& context, const Expr* expr);

        // Helpers
        void addBlockAndSetInsertionPoint(CodeGenContext& context, llvm::BasicBlock* basicBlock);

        // Decls
        llvm::Function* generateFunctionDecl(CodeGenContext& context, const FunctionDecl* functionDecl);

        // Stmts
        void generateCompoundStmt(CodeGenContext& context, const CompoundStmt* compoundStmt);
        void generateReturnStmt(CodeGenContext& context, const ReturnStmt* returnStmt);
        void generateLabeledStmt(CodeGenContext& context, const LabeledStmt* labeledStmt);
        void generateGotoStmt(CodeGenContext& context, const GotoStmt* gotoStmt);
        void generateIfStmt(CodeGenContext& context, const IfStmt* ifStmt);
        void generateWhileStmt(CodeGenContext& context, const WhileStmt* whileStmt, const std::string& loopName);
        void generateDoStmt(CodeGenContext& context, const DoStmt* doStmt, const std::string& loopName);
        void generateForStmt(CodeGenContext& context, const ForStmt* forStmt, const std::string& loopName);
        void generateBreakStmt(CodeGenContext& context, const BreakStmt* breakStmt);
        void generateContinueStmt(CodeGenContext& context, const ContinueStmt* continueStmt);

        // Types
        llvm::Type* generateLlvmType(gulc::CodeGenContext& context, const gulc::Type* type);
        std::vector<llvm::Type*> generateParamTypes(gulc::CodeGenContext& context, const std::vector<ParameterDecl*>& parameters);

        // Exprs
        llvm::Value* generateBinaryOperatorExpr(CodeGenContext& context, const BinaryOperatorExpr* binaryOperatorExpr);
        llvm::Value* generateIntegerLiteralExpr(CodeGenContext& context, const IntegerLiteralExpr* integerLiteralExpr);
        llvm::Value* generateFloatLiteralExpr(CodeGenContext& context, const FloatLiteralExpr* floatLiteralExpr);
        llvm::Value* generateLocalVariableDeclExpr(CodeGenContext& context, const LocalVariableDeclExpr* localVariableDeclExpr);
        llvm::Value* generateIdentifierExpr(CodeGenContext& context, const IdentifierExpr* identifierExpr);
        llvm::Value* generateLValueToRValue(CodeGenContext& context, const LValueToRValueExpr* lValueToRValueExpr);
        llvm::Value* generateFunctionCallExpr(CodeGenContext& context, const FunctionCallExpr* functionCallExpr);
        llvm::Value* generatePrefixOperatorExpr(CodeGenContext& context, const PrefixOperatorExpr* prefixOperatorExpr);
        llvm::Value* generatePostfixOperatorExpr(CodeGenContext& context, const PostfixOperatorExpr* postfixOperatorExpr);

    };
}

#endif //GULC_CODEGEN_HPP
