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

#ifndef GULC_IFSTMT_HPP
#define GULC_IFSTMT_HPP

#include <AST/Stmt.hpp>
#include <AST/Expr.hpp>

namespace gulc {
    class IfStmt : public Stmt {
    public:
        static bool classof(const Stmt *stmt) { return stmt->getStmtKind() == Kind::If; }

        IfStmt(TextPosition startPosition, TextPosition endPosition,
               Expr* condition, Stmt* trueStmt, Stmt* falseStmt)
                : Stmt(Kind::If, startPosition, endPosition),
                  condition(condition), trueStmt(trueStmt), falseStmt(falseStmt) {}

        Expr* condition;
        Stmt* trueStmt;
        Stmt* falseStmt;
        bool hasFalseStmt() const { return falseStmt != nullptr; }

        ~IfStmt() override  {
            delete condition;
            delete trueStmt;
            delete falseStmt;
        }

    };
}

#endif //GULC_IFSTMT_HPP
