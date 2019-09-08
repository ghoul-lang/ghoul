#ifndef GULC_DOSTMT_HPP
#define GULC_DOSTMT_HPP

#include <AST/Stmt.hpp>
#include <AST/Expr.hpp>

namespace gulc {
    class DoStmt : public Stmt {
    public:
        static bool classof(const Stmt *stmt) { return stmt->getStmtKind() == Kind::Do; }

        DoStmt(TextPosition startPosition, TextPosition endPosition,
               Expr* condition, Stmt* loopStmt)
                : Stmt(Kind::Do, startPosition, endPosition),
                  condition(condition), loopStmt(loopStmt) {}

        Expr* condition;
        Stmt* loopStmt;

        ~DoStmt() override {
            delete condition;
            delete loopStmt;
        }

    };
}

#endif //GULC_DOSTMT_HPP
