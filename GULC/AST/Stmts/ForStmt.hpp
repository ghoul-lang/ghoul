#ifndef GULC_FORSTMT_HPP
#define GULC_FORSTMT_HPP

#include <AST/Stmt.hpp>
#include <AST/Expr.hpp>

namespace gulc {
    class ForStmt : public Stmt {
    public:
        static bool classof(const Stmt *stmt) { return stmt->getStmtKind() == StmtKind::For; }

        ForStmt(TextPosition startPosition, TextPosition endPosition,
                Expr* preLoop, Expr* condition, Expr* iterationExpr, Stmt* loopStmt)
                : Stmt(StmtKind::For, startPosition, endPosition),
                  _preLoop(preLoop), _condition(condition), _iterationExpr(iterationExpr), _loopStmt(loopStmt) {}

        const Expr* preLoop() const { return _preLoop; }
        const Expr* condition() const { return _condition; }
        const Expr* iterationExpr() const { return _iterationExpr; }
        const Stmt* loopStmt() const { return _loopStmt; }

        ~ForStmt() override {
            delete _preLoop;
            delete _condition;
            delete _iterationExpr;
            delete _loopStmt;
        }

    private:
        Expr* _preLoop;
        Expr* _condition;
        /// The expression that gets called every loop
        Expr* _iterationExpr;
        Stmt* _loopStmt;

    };
}

#endif //GULC_FORSTMT_HPP
