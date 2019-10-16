// Copyright (C) 2019 Michael Brandon Huddle
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as published
// by the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public License
// along with this program.  If not, see <https://www.gnu.org/licenses/>.

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

        Stmt* deepCopy() const override {
            std::vector<CaseStmt*> copiedCases;

            for (CaseStmt* caseStmt : _cases) {
                copiedCases.push_back(static_cast<CaseStmt*>(caseStmt->deepCopy()));
            }

            return new SwitchStmt(startPosition(), endPosition(), condition->deepCopy(),
                                  std::move(copiedCases));
        }

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
