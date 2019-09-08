#ifndef GULC_CASESTMT_HPP
#define GULC_CASESTMT_HPP

#include <AST/Stmt.hpp>
#include <AST/Expr.hpp>

namespace gulc {
    class CaseStmt : public Stmt {
    public:
        static bool classof(const Stmt *stmt) { return stmt->getStmtKind() == Kind::Case; }

        CaseStmt(TextPosition startPosition, TextPosition endPosition,
                 Expr* condition, Stmt* trueStmt, bool isDefault)
                : Stmt(Kind::Case, startPosition, endPosition),
			      condition(condition), trueStmt(trueStmt), _isDefault(isDefault) {}

        bool isDefault() const { return _isDefault; }
        // If condition is false then we treat the case as a normal statement.
        Expr* condition;
        Stmt* trueStmt;
        bool hasCondition() const { return condition != nullptr; }

        ~CaseStmt() override {
            delete condition;
            delete trueStmt;
        }

    private:
        bool _isDefault;

    };
}

#endif //GULC_CASESTMT_HPP
