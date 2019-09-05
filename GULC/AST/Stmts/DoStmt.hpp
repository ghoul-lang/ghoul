#ifndef GULC_DOSTMT_HPP
#define GULC_DOSTMT_HPP

#include <AST/Stmt.hpp>
#include <AST/Expr.hpp>

namespace gulc {
    class DoStmt : public Stmt {
    public:
        static bool classof(const Stmt *stmt) { return stmt->getStmtKind() == StmtKind::Do; }

        DoStmt(TextPosition startPosition, TextPosition endPosition,
               Expr* condition, Stmt* loopStmt)
                : Stmt(StmtKind::Do, startPosition, endPosition),
                  _condition(condition), _loopStmt(loopStmt) {}

        const Expr* condition() const { return _condition; }
        const Stmt* loopStmt() const { return _loopStmt; }

        ~DoStmt() override {
            delete _condition;
            delete _loopStmt;
        }

    private:
        Expr* _condition;
        Stmt* _loopStmt;

    };
}

#endif //GULC_DOSTMT_HPP
