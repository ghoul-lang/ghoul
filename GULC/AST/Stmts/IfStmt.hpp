#ifndef GULC_IFSTMT_HPP
#define GULC_IFSTMT_HPP

#include <AST/Stmt.hpp>
#include <AST/Expr.hpp>

namespace gulc {
    class IfStmt : public Stmt {
    public:
        static bool classof(const Stmt *stmt) { return stmt->getStmtKind() == Kind::If; }

        IfStmt(TextPosition startPosition, TextPosition endPosition,
               Expr* condition, Stmt* trueStmt, Stmt* falseStmt)
                : Stmt(Kind::If, startPosition, endPosition),
                  condition(condition), trueStmt(trueStmt), falseStmt(falseStmt) {}

        Expr* condition;
        Stmt* trueStmt;
        Stmt* falseStmt;
        bool hasFalseStmt() const { return falseStmt != nullptr; }

        ~IfStmt() override  {
            delete condition;
            delete trueStmt;
            delete falseStmt;
        }

    };
}

#endif //GULC_IFSTMT_HPP
