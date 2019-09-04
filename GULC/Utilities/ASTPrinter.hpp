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
        static void printFunctionDecl(const FunctionDecl* functionDecl, const std::string& prefix = "");

        // Stmts
        static void printCompoundStmt(const CompoundStmt* compoundStmt, const std::string& prefix = "");
        static void printReturnStmt(const ReturnStmt* returnStmt, const std::string& prefix = "");

        // Exprs
        static void printBinaryOperatorExpr(const BinaryOperatorExpr* binaryOperatorExpr, const std::string& prefix = "");
        static void printIdentifierExpr(const IdentifierExpr* identifierExpr, const std::string& prefix = "");
        static void printIntegerLiteralExpr(const IntegerLiteralExpr* integerLiteralExpr, const std::string& prefix = "");
        static void printFunctionCallExpr(const FunctionCallExpr* functionCallExpr, const std::string& prefix = "");
        static void printPrefixOperatorExpr(const PrefixOperatorExpr* prefixOperatorExpr, const std::string& prefix = "");
        static void printPostfixOperatorExpr(const PostfixOperatorExpr* postfixOperatorExpr, const std::string& prefix = "");
        static void printParenExpr(const ParenExpr* parenExpr, const std::string& prefix = "");
        static void printFloatLiteralExpr(const FloatLiteralExpr* floatLiteralExpr, const std::string& prefix = "");

    };
}

#endif //GULC_ASTPRINTER_HPP
