#ifndef GULC_DECLRESOLVERPASS_HPP
#define GULC_DECLRESOLVERPASS_HPP

#include <map>

#include <Passes/FileASTPass.hpp>
#include <AST/Decls/TemplateParameterDecl.hpp>
#include <AST/Decls/FunctionDecl.hpp>
#include <AST/Decls/ParameterDecl.hpp>
#include <AST/Stmts/DoStmt.hpp>
#include <AST/Stmts/ForStmt.hpp>
#include <AST/Stmts/IfStmt.hpp>
#include <AST/Stmts/LabeledStmt.hpp>
#include <AST/Stmts/SwitchStmt.hpp>
#include <AST/Stmts/TryStmt.hpp>
#include <AST/Stmts/TryCatchStmt.hpp>
#include <AST/Stmts/WhileStmt.hpp>
#include <AST/Stmts/ReturnStmt.hpp>
#include <AST/Exprs/BinaryOperatorExpr.hpp>
#include <AST/Exprs/FunctionCallExpr.hpp>
#include <AST/Exprs/IdentifierExpr.hpp>
#include <AST/Exprs/IndexerCallExpr.hpp>
#include <AST/Exprs/MemberAccessCallExpr.hpp>
#include <AST/Exprs/ParenExpr.hpp>
#include <AST/Exprs/PostfixOperatorExpr.hpp>
#include <AST/Exprs/PotentialExplicitCastExpr.hpp>
#include <AST/Exprs/PrefixOperatorExpr.hpp>
#include <AST/Exprs/TernaryExpr.hpp>
#include <AST/Stmts/BreakStmt.hpp>
#include <AST/Stmts/ContinueStmt.hpp>
#include <AST/Stmts/GotoStmt.hpp>
#include <AST/Exprs/CharacterLiteralExpr.hpp>
#include <AST/Exprs/ExplicitCastExpr.hpp>
#include <AST/Exprs/FloatLiteralExpr.hpp>
#include <AST/Exprs/ImplicitCastExpr.hpp>
#include <AST/Exprs/IntegerLiteralExpr.hpp>
#include <AST/Exprs/LocalVariableDeclExpr.hpp>
#include <AST/Exprs/ResolvedTypeRefExpr.hpp>
#include <AST/Exprs/StringLiteralExpr.hpp>

namespace gulc {
    struct ResolveDeclsContext {
        FileAST& fileAst;
        Type* returnType;
        const std::vector<TemplateParameterDecl*>* functionTemplateParams;
        const std::vector<ParameterDecl*>* functionParams;
        const std::vector<Expr*>* functionCallArgs;
        // List of resolved and unresolved labels (if the boolean is true then it is resolved, else it isn't found)
        std::map<std::string, bool> labelNames;

        explicit ResolveDeclsContext(FileAST& fileAst)
                : fileAst(fileAst), returnType(nullptr), functionTemplateParams(nullptr),
			      functionParams(nullptr), functionCallArgs(nullptr), labelNames() {}

        void labelResolved(const std::string& labelName) {
            if (labelNames.find(labelName) != labelNames.end()) {
                labelNames[labelName] = true;
            } else {
                labelNames.insert({ labelName, true });
            }
        }

        void addUnresolvedLabel(const std::string& labelName) {
            if (labelNames.find(labelName) == labelNames.end()) {
                labelNames.insert({ labelName, false });
            }
        }
    };

    // Handles resolving variable calls and function calls to their absolute paths, also handles creating 'ImplicitCastExpr's
    class DeclResolverPass : public FileASTPass {
    public:
        void processFile(FileAST& fileAst) override;

    private:
        bool getTypesAreSame(const Type* type1, const Type* type2);
        bool getTypeGreaterThan(const Type* left, const Type* right);

        Type* deepCopyAndSimplifyType(const Type* type);

        void printError(const std::string& message, FileAST &fileAst, TextPosition startPosition, TextPosition endPosition);
        void printDebugWarning(const std::string& message, FileAST &fileAst);

        void processDecl(FileAST &fileAst, Decl* decl);
        void processStmt(ResolveDeclsContext& context, Stmt*& stmt);
        void processExpr(ResolveDeclsContext& context, Expr*& expr);

        void processFunctionDecl(FileAST &fileAst, FunctionDecl* functionDecl);

        void processBreakStmt(ResolveDeclsContext& context, BreakStmt* breakStmt);
        void processCaseStmt(ResolveDeclsContext& context, CaseStmt* caseStmt);
        void processCompoundStmt(ResolveDeclsContext& context, CompoundStmt* compoundStmt);
        void processContinueStmt(ResolveDeclsContext& context, ContinueStmt* continueStmt);
        void processDoStmt(ResolveDeclsContext& context, DoStmt* doStmt);
        void processForStmt(ResolveDeclsContext& context, ForStmt* forStmt);
        void processGotoStmt(ResolveDeclsContext& context, GotoStmt* gotoStmt);
        void processIfStmt(ResolveDeclsContext& context, IfStmt* ifStmt);
        void processLabeledStmt(ResolveDeclsContext& context, LabeledStmt* labeledStmt);
        void processReturnStmt(ResolveDeclsContext& context, ReturnStmt* returnStmt);
        void processSwitchStmt(ResolveDeclsContext& context, SwitchStmt* switchStmt);
        void processTryStmt(ResolveDeclsContext& context, TryStmt* tryStmt);
        void processTryCatchStmt(ResolveDeclsContext& context, TryCatchStmt* tryCatchStmt);
        void processTryFinallyStmt(ResolveDeclsContext& context, TryFinallyStmt* tryFinallyStmt);
        void processWhileStmt(ResolveDeclsContext& context, WhileStmt* whileStmt);

        void processBinaryOperatorExpr(ResolveDeclsContext& context, BinaryOperatorExpr* binaryOperatorExpr);
        void processCharacterLiteralExpr(ResolveDeclsContext& context, CharacterLiteralExpr* characterLiteralExpr);
        void processExplicitCastExpr(ResolveDeclsContext& context, ExplicitCastExpr* explicitCastExpr);
        void processFloatLiteralExpr(ResolveDeclsContext& context, FloatLiteralExpr* floatLiteralExpr);
        void processFunctionCallExpr(ResolveDeclsContext& context, FunctionCallExpr* functionCallExpr);
        void processIdentifierExpr(ResolveDeclsContext& context, IdentifierExpr* identifierExpr);
        void processImplicitCastExpr(ResolveDeclsContext& context, ImplicitCastExpr* implicitCastExpr);
        void processIndexerCallExpr(ResolveDeclsContext& context, IndexerCallExpr* indexerCallExpr);
        void processIntegerLiteralExpr(ResolveDeclsContext& context, IntegerLiteralExpr* integerLiteralExpr);
        void processLocalVariableDeclExpr(ResolveDeclsContext& context, LocalVariableDeclExpr* localVariableDeclExpr);
        void processLocalVariableDeclOrPrefixOperatorCallExpr(ResolveDeclsContext& context, Expr*& expr);
        void processMemberAccessCallExpr(ResolveDeclsContext& context, MemberAccessCallExpr* memberAccessCallExpr);
        void processParenExpr(ResolveDeclsContext& context, ParenExpr* parenExpr);
        void processPostfixOperatorExpr(ResolveDeclsContext& context, PostfixOperatorExpr* postfixOperatorExpr);
        void processPotentialExplicitCastExpr(ResolveDeclsContext& context, PotentialExplicitCastExpr* potentialExplicitCastExpr);
        void processPrefixOperatorExpr(ResolveDeclsContext& context, PrefixOperatorExpr* prefixOperatorExpr);
        void processResolvedTypeRefExpr(ResolveDeclsContext& context, ResolvedTypeRefExpr* resolvedTypeRefExpr);
        void processStringLiteralExpr(ResolveDeclsContext& context, StringLiteralExpr* stringLiteralExpr);
        void processTernaryExpr(ResolveDeclsContext& context, TernaryExpr* ternaryExpr);
        void processUnresolvedTypeRefExpr(ResolveDeclsContext& context, Expr*& expr);

    };
}

#endif //GULC_DECLRESOLVERPASS_HPP
