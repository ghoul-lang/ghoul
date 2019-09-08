#ifndef GULC_SWITCHSTMT_HPP
#define GULC_SWITCHSTMT_HPP

#include <AST/Stmt.hpp>
#include <AST/Expr.hpp>
#include <vector>
#include "CaseStmt.hpp"

namespace gulc {
    class SwitchStmt : public Stmt {
    public:
        static bool classof(const Stmt *stmt) { return stmt->getStmtKind() == Kind::Switch; }

        SwitchStmt(TextPosition startPosition, TextPosition endPosition,
                   Expr* condition, std::vector<CaseStmt*> cases)
                : Stmt(Kind::Switch, startPosition, endPosition),
                  condition(condition), _cases(std::move(cases)) {}

        Expr* condition;
        std::vector<CaseStmt*>& cases() { return _cases; }
        const std::vector<CaseStmt*>& cases() const { return _cases; }

        ~SwitchStmt() override {
            delete condition;

            for (CaseStmt* caseStmt : _cases) {
                delete caseStmt;
            }
        }

    private:
        std::vector<CaseStmt*> _cases;

    };
}

#endif //GULC_SWITCHSTMT_HPP
