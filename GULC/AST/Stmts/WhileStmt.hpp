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

#ifndef GULC_WHILESTMT_HPP
#define GULC_WHILESTMT_HPP

#include <AST/Stmt.hpp>
#include <AST/Expr.hpp>

namespace gulc {
    class WhileStmt : public Stmt {
    public:
        static bool classof(const Stmt *stmt) { return stmt->getStmtKind() == Kind::While; }

        WhileStmt(TextPosition startPosition, TextPosition endPosition,
                  Expr* condition, Stmt* loopStmt)
                : Stmt(Kind::While, startPosition, endPosition),
                  condition(condition), loopStmt(loopStmt) {}

        Expr* condition;
        Stmt* loopStmt;

        Stmt* deepCopy() const override {
            return new WhileStmt(startPosition(), endPosition(),
                                 condition->deepCopy(), loopStmt->deepCopy());
        }

        ~WhileStmt() override {
            delete condition;
            delete loopStmt;
        }

    };
}

#endif //GULC_WHILESTMT_HPP
