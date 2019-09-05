#ifndef GULC_WHILESTMT_HPP
#define GULC_WHILESTMT_HPP

#include <AST/Stmt.hpp>
#include <AST/Expr.hpp>

namespace gulc {
    class WhileStmt : public Stmt {
    public:
        static bool classof(const Stmt *stmt) { return stmt->getStmtKind() == StmtKind::While; }

        WhileStmt(TextPosition startPosition, TextPosition endPosition,
                  Expr* condition, Stmt* loopStmt)
                : Stmt(StmtKind::While, startPosition, endPosition),
                  _condition(condition), _loopStmt(loopStmt) {}

        const Expr* condition() const { return _condition; }
        const Stmt* loopStmt() const { return _loopStmt; }

        ~WhileStmt() override {
            delete _condition;
            delete _loopStmt;
        }

    private:
        Expr* _condition;
        Stmt* _loopStmt;

    };
}

#endif //GULC_WHILESTMT_HPP
