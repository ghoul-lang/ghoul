#ifndef GULC_FORSTMT_HPP
#define GULC_FORSTMT_HPP

#include <AST/Stmt.hpp>
#include <AST/Expr.hpp>

namespace gulc {
    class ForStmt : public Stmt {
    public:
        static bool classof(const Stmt *stmt) { return stmt->getStmtKind() == Kind::For; }

        ForStmt(TextPosition startPosition, TextPosition endPosition,
                Expr* preLoop, Expr* condition, Expr* iterationExpr, Stmt* loopStmt)
                : Stmt(Kind::For, startPosition, endPosition),
                  preLoop(preLoop), condition(condition), iterationExpr(iterationExpr), loopStmt(loopStmt) {}

        Expr* preLoop;
        Expr* condition;
        /// The expression that gets called every loop
        Expr* iterationExpr;
        Stmt* loopStmt;

        ~ForStmt() override {
            delete preLoop;
            delete condition;
            delete iterationExpr;
            delete loopStmt;
        }

    };
}

#endif //GULC_FORSTMT_HPP
