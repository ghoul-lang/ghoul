#ifndef GULC_LIFETIMES_HPP
#define GULC_LIFETIMES_HPP

#include <AST/Decl.hpp>
#include <AST/Stmt.hpp>
#include <AST/Expr.hpp>
#include <AST/Decls/ConstructorDecl.hpp>
#include <AST/Decls/FunctionDecl.hpp>
#include <AST/Decls/TemplateFunctionDecl.hpp>
#include <AST/Stmts/BreakStmt.hpp>
#include <AST/Stmts/CaseStmt.hpp>
#include <AST/Stmts/ContinueStmt.hpp>
#include <AST/Stmts/DoStmt.hpp>
#include <AST/Stmts/ForStmt.hpp>
#include <AST/Stmts/GotoStmt.hpp>
#include <AST/Stmts/IfStmt.hpp>
#include <AST/Stmts/LabeledStmt.hpp>
#include <AST/Stmts/ReturnStmt.hpp>
#include <AST/Stmts/SwitchStmt.hpp>
#include <AST/Stmts/TryStmt.hpp>
#include <AST/Stmts/WhileStmt.hpp>
#include <AST/Exprs/CharacterLiteralExpr.hpp>
#include <AST/Exprs/ExplicitCastExpr.hpp>
#include <AST/Exprs/FunctionCallExpr.hpp>
#include <AST/Exprs/IdentifierExpr.hpp>
#include <AST/Exprs/RefGlobalVariableExpr.hpp>
#include <AST/Exprs/ImplicitCastExpr.hpp>
#include <AST/Exprs/IndexerCallExpr.hpp>
#include <AST/Exprs/LocalVariableDeclExpr.hpp>
#include <AST/Exprs/ParenExpr.hpp>
#include <AST/Exprs/PostfixOperatorExpr.hpp>
#include <AST/Exprs/PrefixOperatorExpr.hpp>
#include <AST/Exprs/StringLiteralExpr.hpp>
#include <AST/Exprs/TernaryExpr.hpp>
#include <AST/Exprs/MemberAccessCallExpr.hpp>
#include <AST/Exprs/DestructLocalVariableExpr.hpp>
#include <AST/FileAST.hpp>

// TODO: We need to support destructors in situations where we use `goto` and `break [label];`/`continue [label];`
//  in these situations we will have to find a common `CompoundStmt` parent that we traverse back to when finding the
//  correct number of variables to destruct...

// TODO: We also need to support destructors when we `return`. This will destruct ALL local variables

// TODO: We need to destruct parameters on return too

// TODO: We should also check `goto` for when we try to `goto` somewhere that might have variables declared before the
//  label that don't exist from where we `goto`. We MIGHT be able to do this by storing the number of local variables
//  that exist in the context of the `LabelStmt` and checking that it is equal to the number of local variables that
//  currently exist within the common parent. If the `goto` has more local variables then there is an error...

// TODO: We might want to consider not allowing `goto` to jump from one child to another. Only allow jumping from
//  a child to the same child or a child to a parent or grandparent. This would simplify the destructor call generation

namespace gulc {
    /**
     * The `Lifetimes` pass handles resolving empty constructors, placing/calling destructors,
     * verifying accessed variables have been initialized,
     * and will at some point handle `lifetime` verifications
     */
    class Lifetimes {
    public:
        Lifetimes()
                : currentFileAst(nullptr), currentFunction(nullptr), currentNamespace(nullptr),
                  currentStruct(nullptr), currentLocalVariablesCount(0), currentLocalVariables(),
                  exprTemporaryObjects(), currentLoop(nullptr) {}

        void processFile(std::vector<FileAST*>& files);

    private:
        void printError(const std::string& message, TextPosition startPosition, TextPosition endPosition);

        void processDecl(Decl* decl);
        void processStmt(Stmt*& stmt);
        void processExpr(Expr*& expr);

        void processConstructorDecl(ConstructorDecl* constructorDecl);
        void processDestructorDecl(DestructorDecl* destructorDecl);
        void processFunctionDecl(FunctionDecl* functionDecl);
        // TODO: Should we process global variables?
        void processNamespaceDecl(NamespaceDecl* namespaceDecl);
        void processStructDecl(StructDecl* structDecl);
        void processTemplateFunctionDecl(TemplateFunctionDecl* templateFunctionDecl);
        void processTemplateFunctionDeclImplementation(TemplateFunctionDecl* templateFunctionDecl,
                                                       FunctionDecl* implementedFunction);

        void processBreakStmt(BreakStmt* breakStmt);
        void processCaseStmt(CaseStmt* caseStmt);
        void processCompoundStmt(CompoundStmt* compoundStmt, bool isFunctionBody);
        void processContinueStmt(ContinueStmt* continueStmt);
        void processDoStmt(DoStmt* doStmt);
        void processForStmt(ForStmt* forStmt);
        void processGotoStmt(GotoStmt* gotoStmt);
        void processIfStmt(IfStmt* ifStmt);
        void processLabeledStmt(LabeledStmt* labeledStmt);
        void processReturnStmt(ReturnStmt* returnStmt);
        void processSwitchStmt(SwitchStmt* switchStmt);
        void processTryStmt(TryStmt* tryStmt);
        void processTryCatchStmt(TryCatchStmt* tryCatchStmt);
        void processTryFinallyStmt(TryFinallyStmt* tryFinallyStmt);
        void processWhileStmt(WhileStmt* whileStmt);

        void processBinaryOperatorExpr(Expr*& expr);
        void processCharacterLiteralExpr(CharacterLiteralExpr* characterLiteralExpr);
        void processExplicitCastExpr(ExplicitCastExpr* explicitCastExpr);
        void processFloatLiteralExpr(FloatLiteralExpr* floatLiteralExpr);
        void processFunctionCallExpr(FunctionCallExpr* functionCallExpr);
        void processIdentifierExpr(Expr*& identifierExpr);
        void processImplicitCastExpr(ImplicitCastExpr* implicitCastExpr);
        void processIndexerCallExpr(IndexerCallExpr* indexerCallExpr);
        void processIntegerLiteralExpr(IntegerLiteralExpr* integerLiteralExpr);
        void processLocalVariableDeclExpr(LocalVariableDeclExpr* localVariableDeclExpr);
        void processMemberAccessCallExpr(MemberAccessCallExpr* memberAccessCallExpr);
        void processParenExpr(ParenExpr* parenExpr);
        void processPostfixOperatorExpr(PostfixOperatorExpr* postfixOperatorExpr);
        void processPrefixOperatorExpr(PrefixOperatorExpr* prefixOperatorExpr);
        void processStringLiteralExpr(StringLiteralExpr* stringLiteralExpr);
        void processTernaryExpr(TernaryExpr* ternaryExpr);

        DestructLocalVariableExpr* destructLocalVariable(LocalVariableDeclExpr* localVariableDeclExpr);
        void destructLocalVariablesDeclaredAfterLoop(Stmt* loop, std::vector<Expr*>& addToList);

        // Context members
        FileAST* currentFileAst;
        FunctionDecl* currentFunction;
        NamespaceDecl* currentNamespace;
        StructDecl* currentStruct;
        unsigned int currentLocalVariablesCount;
        std::vector<LocalVariableDeclExpr*> currentLocalVariables;
        // Temporary objects created during a full expression, these need destructed after the full expression is
        // complete
        std::vector<LocalVariableDeclExpr*> exprTemporaryObjects;
        Stmt* currentLoop;

        // Add local variable to the current local variables, these are used to keep track of variables we will have to
        // destruct
        void trackLocalVariable(LocalVariableDeclExpr* localVariableDeclExpr) {
            if (currentLocalVariablesCount >= currentLocalVariables.size()) {
                currentLocalVariables.push_back(localVariableDeclExpr);
            } else {
                currentLocalVariables[currentLocalVariablesCount] = localVariableDeclExpr;
            }

            ++currentLocalVariablesCount;
        }

    };
}

#endif //GULC_LIFETIMES_HPP
