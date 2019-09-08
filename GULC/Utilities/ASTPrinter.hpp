#ifndef GULC_ASTPRINTER_HPP
#define GULC_ASTPRINTER_HPP

#include <AST/Decl.hpp>
#include <AST/Stmt.hpp>
#include <AST/Expr.hpp>
#include <AST/Types/UnresolvedType.hpp>
#include <AST/Exprs/BinaryOperatorExpr.hpp>
#include <AST/Exprs/IdentifierExpr.hpp>
#include <AST/Stmts/CompoundStmt.hpp>
#include <AST/Stmts/ReturnStmt.hpp>
#include <AST/Decls/FunctionDecl.hpp>
#include <AST/Exprs/IntegerLiteralExpr.hpp>
#include <AST/Exprs/FunctionCallExpr.hpp>
#include <AST/Exprs/PrefixOperatorExpr.hpp>
#include <AST/Exprs/PostfixOperatorExpr.hpp>
#include <AST/Exprs/ParenExpr.hpp>
#include <AST/Exprs/FloatLiteralExpr.hpp>
#include <AST/Exprs/PotentialExplicitCastExpr.hpp>
#include <AST/Exprs/LocalVariableDeclOrPrefixOperatorCallExpr.hpp>
#include <AST/Exprs/IndexerCallExpr.hpp>
#include <AST/Stmts/LabeledStmt.hpp>
#include <AST/Stmts/IfStmt.hpp>
#include <AST/Stmts/WhileStmt.hpp>
#include <AST/Stmts/ForStmt.hpp>
#include <AST/Stmts/DoStmt.hpp>
#include <AST/Stmts/SwitchStmt.hpp>
#include <AST/Stmts/BreakStmt.hpp>
#include <AST/Stmts/ContinueStmt.hpp>
#include <AST/Stmts/GotoStmt.hpp>
#include <AST/Stmts/TryStmt.hpp>
#include <AST/Exprs/CharacterLiteralExpr.hpp>
#include <AST/Exprs/StringLiteralExpr.hpp>
#include <AST/Exprs/MemberAccessCallExpr.hpp>
#include <AST/Exprs/TernaryExpr.hpp>
#include <AST/Exprs/LocalVariableDeclExpr.hpp>
#include <AST/Exprs/ResolvedTypeRefExpr.hpp>

namespace gulc {
    class ASTPrinter {
    public:
        static void printDecl(const Decl* decl, const std::string& prefix = "");
        static void printStmt(const Stmt* stmt, const std::string& prefix = "");
        static void printExpr(const Expr* expr, const std::string& prefix = "");

        // Types
        static std::string getTypeName(const Type* type);

        // Decls
        static std::string getParametersString(const std::vector<ParameterDecl*>& parameters);
        static std::string getTemplateParametersString(const std::vector<TemplateParameterDecl*>& templateParameters);
        static void printFunctionDecl(const FunctionDecl* functionDecl, const std::string& prefix = "");

        // Stmts
        static void printCompoundStmt(const CompoundStmt* compoundStmt, const std::string& prefix = "");
        static void printReturnStmt(const ReturnStmt* returnStmt, const std::string& prefix = "");
        static void printLabeledStmt(const LabeledStmt* labeledStmt, const std::string& prefix = "");
        static void printIfStmt(const IfStmt* ifStmt, const std::string& prefix = "");
        static void printWhileStmt(const WhileStmt* whileStmt, const std::string& prefix = "");
        static void printForStmt(const ForStmt* forStmt, const std::string& prefix = "");
        static void printDoStmt(const DoStmt* doStmt, const std::string& prefix = "");
        static void printSwitchStmt(const SwitchStmt* switchStmt, const std::string& prefix = "");
        static void printCaseStmt(const CaseStmt* caseStmt, const std::string& prefix = "");
        static void printBreakStmt(const BreakStmt* breakStmt, const std::string& prefix = "");
        static void printContinueStmt(const ContinueStmt* continueStmt, const std::string& prefix = "");
        static void printGotoStmt(const GotoStmt* gotoStmt, const std::string& prefix = "");
        static void printTryStmt(const TryStmt* tryStmt, const std::string& prefix = "");
        static void printTryCatchStmt(const TryCatchStmt* tryCatchStmt, const std::string& prefix = "");
        static void printTryFinallyStmt(const TryFinallyStmt* tryFinallyStmt, const std::string& prefix = "");

        // Exprs
        static void printBinaryOperatorExpr(const BinaryOperatorExpr* binaryOperatorExpr, const std::string& prefix = "");
        static void printIdentifierExpr(const IdentifierExpr* identifierExpr, const std::string& prefix = "");
        static void printIntegerLiteralExpr(const IntegerLiteralExpr* integerLiteralExpr, const std::string& prefix = "");
        static void printFunctionCallExpr(const FunctionCallExpr* functionCallExpr, const std::string& prefix = "");
        static void printPrefixOperatorExpr(const PrefixOperatorExpr* prefixOperatorExpr, const std::string& prefix = "");
        static void printPostfixOperatorExpr(const PostfixOperatorExpr* postfixOperatorExpr, const std::string& prefix = "");
        static void printParenExpr(const ParenExpr* parenExpr, const std::string& prefix = "");
        static void printFloatLiteralExpr(const FloatLiteralExpr* floatLiteralExpr, const std::string& prefix = "");
        static void printPotentialExplicitCastExpr(const PotentialExplicitCastExpr* potentialExplicitCastExpr, const std::string& prefix = "");
        static void printLocalVariableDeclOrPrefixOperatorCallExpr(const LocalVariableDeclOrPrefixOperatorCallExpr* localVariableDeclOrPrefixOperatorCallExpr, const std::string& prefix = "");
        static void printIndexerCallExpr(const IndexerCallExpr* indexerCallExpr, const std::string& prefix = "");
        static void printCharacterLiteralExpr(const CharacterLiteralExpr* characterLiteralExpr, const std::string& prefix = "");
        static void printStringLiteralExpr(const StringLiteralExpr* stringLiteralExpr, const std::string& prefix = "");
        static void printMemberAccessCallExpr(const MemberAccessCallExpr* memberAccessCallExpr, const std::string& prefix = "");
        static void printTernaryExpr(const TernaryExpr* ternaryExpr, const std::string& prefix = "");
        static void printLocalVariableDeclExpr(const LocalVariableDeclExpr* localVariableDeclExpr, const std::string& prefix = "");
        static void printResolvedTypeRefExpr(const ResolvedTypeRefExpr* resolvedTypeRefExpr, const std::string& prefix = "");

    };
}

#endif //GULC_ASTPRINTER_HPP
