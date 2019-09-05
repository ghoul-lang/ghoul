#ifndef GULC_IFSTMT_HPP
#define GULC_IFSTMT_HPP

#include <AST/Stmt.hpp>
#include <AST/Expr.hpp>

namespace gulc {
    class IfStmt : public Stmt {
    public:
        static bool classof(const Stmt *stmt) { return stmt->getStmtKind() == StmtKind::If; }

        IfStmt(TextPosition startPosition, TextPosition endPosition,
               Expr* condition, Stmt* trueStmt, Stmt* falseStmt)
                : Stmt(StmtKind::If, startPosition, endPosition),
                  _condition(condition), _trueStmt(trueStmt), _falseStmt(falseStmt) {}

        const Expr* condition() const { return _condition; }
        const Stmt* trueStmt() const { return _trueStmt; }
        const Stmt* falseStmt() const { return _falseStmt; }
        bool hasFalseStmt() const { return _falseStmt != nullptr; }

        ~IfStmt() override  {
            delete _condition;
            delete _trueStmt;
            delete _falseStmt;
        }

    private:
        Expr* _condition;
        Stmt* _trueStmt;
        Stmt* _falseStmt;

    };
}

#endif //GULC_IFSTMT_HPP
