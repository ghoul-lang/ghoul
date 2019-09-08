#ifndef GULC_TYPERESOLVERPASS_HPP
#define GULC_TYPERESOLVERPASS_HPP

#include <Passes/FileASTPass.hpp>
#include <AST/Decl.hpp>
#include <AST/Stmt.hpp>
#include <AST/Expr.hpp>
#include <AST/Decls/FunctionDecl.hpp>
#include <AST/Stmts/DoStmt.hpp>
#include <AST/Stmts/ForStmt.hpp>
#include <AST/Stmts/IfStmt.hpp>
#include <AST/Stmts/LabeledStmt.hpp>
#include <AST/Stmts/SwitchStmt.hpp>
#include <AST/Stmts/TryStmt.hpp>
#include <AST/Stmts/WhileStmt.hpp>
#include <AST/Stmts/ReturnStmt.hpp>
#include <AST/Exprs/BinaryOperatorExpr.hpp>
#include <AST/Exprs/FunctionCallExpr.hpp>
#include <AST/Exprs/IdentifierExpr.hpp>
#include <AST/Exprs/IndexerCallExpr.hpp>
#include <AST/Exprs/LocalVariableDeclOrPrefixOperatorCallExpr.hpp>
#include <AST/Exprs/MemberAccessCallExpr.hpp>
#include <AST/Exprs/PrefixOperatorExpr.hpp>
#include <AST/Exprs/ParenExpr.hpp>
#include <AST/Exprs/PostfixOperatorExpr.hpp>
#include <AST/Exprs/PotentialExplicitCastExpr.hpp>
#include <AST/Exprs/TernaryExpr.hpp>
#include <AST/Exprs/UnresolvedTypeRefExpr.hpp>

namespace gulc {
    struct ResolveTypesContext {
        FileAST& fileAst;
        const std::vector<TemplateParameterDecl*>* functionTemplateParams;

        explicit ResolveTypesContext(FileAST& fileAst)
                : fileAst(fileAst), functionTemplateParams(nullptr) {}
    };

    // The 'TypeResolverPass' isn't related to TypeQualifiers it is related to taking 'RandomClass' and turning that into the absolute path 'projectNamespaces.RandomClass'
    class TypeResolverPass : public FileASTPass {
    public:
        void processFile(FileAST& fileAst) override;

    private:
        bool resolveType(ResolveTypesContext& context, Type*& type);

        void printError(const std::string& message, FileAST &fileAst, TextPosition startPosition, TextPosition endPosition);
        void printDebugWarning(const std::string& message, FileAST &fileAst);

        void processDecl(FileAST &fileAst, Decl* decl);
        void processStmt(ResolveTypesContext& context, Stmt*& stmt);
        void processExpr(ResolveTypesContext& context, Expr*& expr);
        void processTypeOrExpr(ResolveTypesContext& context, Expr*& expr);

        void processFunctionDecl(FileAST &fileAst, FunctionDecl* functionDecl);

        void processCompoundStmt(ResolveTypesContext& context, CompoundStmt* compoundStmt);
        void processDoStmt(ResolveTypesContext& context, DoStmt* doStmt);
        void processForStmt(ResolveTypesContext& context, ForStmt* forStmt);
        void processIfStmt(ResolveTypesContext& context, IfStmt* ifStmt);
        void processLabeledStmt(ResolveTypesContext& context, LabeledStmt* labeledStmt);
        void processSwitchStmt(ResolveTypesContext& context, SwitchStmt* switchStmt);
        void processTryStmt(ResolveTypesContext& context, TryStmt* tryStmt);
        void processTryCatchStmt(ResolveTypesContext& context, TryCatchStmt* tryCatchStmt);
        void processTryFinallyStmt(ResolveTypesContext& context, TryFinallyStmt* tryFinallyStmt);
        void processWhileStmt(ResolveTypesContext& context, WhileStmt* whileStmt);
        void processCaseStmt(ResolveTypesContext& context, CaseStmt* caseStmt);
        void processReturnStmt(ResolveTypesContext& context, ReturnStmt* returnStmt);

        void processBinaryOperatorExpr(ResolveTypesContext& context, BinaryOperatorExpr* binaryOperatorExpr);
        void processFunctionCallExpr(ResolveTypesContext& context, FunctionCallExpr* functionCallExpr);
        void processIdentifierExpr(ResolveTypesContext& context, IdentifierExpr* identifierExpr);
        void processIndexerCallExpr(ResolveTypesContext& context, IndexerCallExpr* indexerCallExpr);
        void processLocalVariableDeclOrPrefixOperatorCallExpr(ResolveTypesContext& context, Expr*& expr);
        void processMemberAccessCallExpr(ResolveTypesContext& context, MemberAccessCallExpr* memberAccessCallExpr);
        void processParenExpr(ResolveTypesContext& context, ParenExpr* parenExpr);
        void processPostfixOperatorExpr(ResolveTypesContext& context, PostfixOperatorExpr* postfixOperatorExpr);
        void processPotentialExplicitCastExpr(ResolveTypesContext& context, PotentialExplicitCastExpr* potentialExplicitCastExpr);
        void processPrefixOperatorExpr(ResolveTypesContext& context, PrefixOperatorExpr* prefixOperatorExpr);
        void processTernaryExpr(ResolveTypesContext& context, TernaryExpr* ternaryExpr);
        void processUnresolvedTypeRefExpr(ResolveTypesContext& context, Expr*& expr);

    };
}

#endif //GULC_TYPERESOLVERPASS_HPP
