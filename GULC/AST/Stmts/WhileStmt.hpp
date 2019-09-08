#ifndef GULC_WHILESTMT_HPP
#define GULC_WHILESTMT_HPP

#include <AST/Stmt.hpp>
#include <AST/Expr.hpp>

namespace gulc {
    class WhileStmt : public Stmt {
    public:
        static bool classof(const Stmt *stmt) { return stmt->getStmtKind() == Kind::While; }

        WhileStmt(TextPosition startPosition, TextPosition endPosition,
                  Expr* condition, Stmt* loopStmt)
                : Stmt(Kind::While, startPosition, endPosition),
                  condition(condition), loopStmt(loopStmt) {}

        Expr* condition;
        Stmt* loopStmt;

        ~WhileStmt() override {
            delete condition;
            delete loopStmt;
        }

    };
}

#endif //GULC_WHILESTMT_HPP
