#ifndef GULC_CASESTMT_HPP
#define GULC_CASESTMT_HPP

#include <AST/Stmt.hpp>
#include <AST/Expr.hpp>

namespace gulc {
    class CaseStmt : public Stmt {
    public:
        static bool classof(const Stmt *stmt) { return stmt->getStmtKind() == StmtKind::Case; }

        CaseStmt(TextPosition startPosition, TextPosition endPosition,
                 Expr* condition, Stmt* trueStmt, bool isDefault)
                : Stmt(StmtKind::Case, startPosition, endPosition),
                  _condition(condition), _trueStmt(trueStmt), _isDefault(isDefault) {}

        bool isDefault() const { return _isDefault; }
        const Expr* condition() const { return _condition; }
        const Stmt* trueStmt() const { return _trueStmt; }

        ~CaseStmt() override {
            delete _condition;
            delete _trueStmt;
        }

    private:
        bool _isDefault;
        // If condition is false then we treat the case as a normal statement.
        Expr* _condition;
        Stmt* _trueStmt;

    };
}

#endif //GULC_CASESTMT_HPP
