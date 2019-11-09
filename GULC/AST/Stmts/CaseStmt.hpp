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

        Stmt* deepCopy() const override {
            auto result = new CaseStmt(startPosition(), endPosition(),
                                       condition->deepCopy(), trueStmt->deepCopy(),
                                       _isDefault);
            result->isUnreachable = isUnreachable;
            return result;
        }

        ~CaseStmt() override {
            delete condition;
            delete trueStmt;
        }

    private:
        bool _isDefault;

    };
}

#endif //GULC_CASESTMT_HPP
