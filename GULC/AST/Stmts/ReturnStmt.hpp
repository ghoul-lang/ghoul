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

#ifndef GULC_RETURNSTMT_HPP
#define GULC_RETURNSTMT_HPP

#include <AST/Stmt.hpp>
#include <AST/Expr.hpp>

namespace gulc {
    class ReturnStmt : public Stmt {
    public:
        static bool classof(const Stmt *stmt) { return stmt->getStmtKind() == Kind::Return; }

        ReturnStmt(TextPosition startPosition, TextPosition endPosition, Expr* returnValue = nullptr)
                : Stmt(Kind::Return, startPosition, endPosition), returnValue(returnValue) {}

        Expr* returnValue;
        bool hasReturnValue() const { return returnValue != nullptr; }

        ~ReturnStmt() override {
            delete returnValue;
        }

    private:

    };
}

#endif //GULC_RETURNSTMT_HPP
