#ifndef GULC_SWITCHSTMT_HPP
#define GULC_SWITCHSTMT_HPP

#include <AST/Stmt.hpp>
#include <AST/Expr.hpp>
#include <vector>
#include "CaseStmt.hpp"

namespace gulc {
    class SwitchStmt : public Stmt {
    public:
        static bool classof(const Stmt *stmt) { return stmt->getStmtKind() == StmtKind::Switch; }

        SwitchStmt(TextPosition startPosition, TextPosition endPosition,
                   Expr* condition, std::vector<CaseStmt*> cases)
                : Stmt(StmtKind::Switch, startPosition, endPosition),
                  _condition(condition), _cases(std::move(cases)) {}

        const Expr* condition() const { return _condition; }
        const std::vector<CaseStmt*>& cases() const { return _cases; }

        ~SwitchStmt() override {
            delete _condition;

            for (CaseStmt* caseStmt : _cases) {
                delete caseStmt;
            }
        }

    private:
        Expr* _condition;
        std::vector<CaseStmt*> _cases;

    };
}

#endif //GULC_SWITCHSTMT_HPP
