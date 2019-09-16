#ifndef GULC_CODEGEN_HPP
#define GULC_CODEGEN_HPP

#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/IRBuilder.h>

#include <AST/FileAST.hpp>
#include <AST/Decls/FunctionDecl.hpp>
#include <AST/Exprs/BinaryOperatorExpr.hpp>
#include <AST/Exprs/IntegerLiteralExpr.hpp>
#include <AST/Exprs/FloatLiteralExpr.hpp>
#include <AST/Stmts/ReturnStmt.hpp>
#include <AST/Stmts/LabeledStmt.hpp>
#include <AST/Stmts/GotoStmt.hpp>

namespace gulc {
    struct CodeGenContext {
        FileAST& fileAst;
        llvm::LLVMContext& llvmContext;
        llvm::IRBuilder<>& irBuilder;
        llvm::Module* module;

        llvm::Function* currentFunction;

        std::map<std::string, llvm::BasicBlock*> currentFunctionLabels;

        CodeGenContext(FileAST& fileAst, llvm::LLVMContext& llvmContext, llvm::IRBuilder<>& irBuilder,
                       llvm::Module* module)
                : fileAst(fileAst), llvmContext(llvmContext), irBuilder(irBuilder), module(module),
                  currentFunction(nullptr), currentFunctionLabels() {}

        bool currentFunctionLabelsContains(const std::string& labelName) {
            return currentFunctionLabels.find(labelName) != currentFunctionLabels.end();
        }

        void addCurrentFunctionLabel(const std::string& labelName, llvm::BasicBlock* basicBlock) {
            if (currentFunctionLabels.find(labelName) == currentFunctionLabels.end()) {
                currentFunctionLabels.insert({labelName, basicBlock});
            }
        }
    };

    class CodeGen {
    public:
        void generate(FileAST& file);

    private:
        void printError(const std::string& message, FileAST &fileAst, TextPosition startPosition, TextPosition endPosition);

        llvm::GlobalObject* generateDecl(CodeGenContext& context, const Decl* decl);
        void generateStmt(CodeGenContext& context, const Stmt* stmt);
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

        // Types
        llvm::Type* generateLlvmType(gulc::CodeGenContext& context, const gulc::Type* type);
        std::vector<llvm::Type*> generateParamTypes(gulc::CodeGenContext& context, const std::vector<ParameterDecl*>& parameters);

        // Exprs
        llvm::Value* generateBinaryOperatorExpr(CodeGenContext& context, const BinaryOperatorExpr* binaryOperatorExpr);
        llvm::Value* generateIntegerLiteralExpr(CodeGenContext& context, const IntegerLiteralExpr* integerLiteralExpr);
        llvm::Value* generateFloatLiteralExpr(CodeGenContext& context, const FloatLiteralExpr* floatLiteralExpr);

    };
}

#endif //GULC_CODEGEN_HPP
