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

#ifndef GULC_CONSTRUCTSTRUCTMEMBERVARIABLESTMT_HPP
#define GULC_CONSTRUCTSTRUCTMEMBERVARIABLESTMT_HPP

#include <AST/Stmt.hpp>

namespace gulc {
    class ConstructorDecl;
    class GlobalVariableDecl;

    class ConstructStructMemberVariableStmt : public Stmt {
    public:
        static bool classof(const Stmt *stmt) { return stmt->getStmtKind() == Kind::ConstructStructMemberVariable; }

        ConstructStructMemberVariableStmt(TextPosition startPosition, TextPosition endPosition,
                                          GlobalVariableDecl* refMemberVariable, ConstructorDecl* constructorDecl)
                : Stmt(Kind::ConstructStructMemberVariable, startPosition, endPosition),
                  refMemberVariable(refMemberVariable), constructorDecl(constructorDecl) {}

        // We don't own this so we don't free it
        GlobalVariableDecl* refMemberVariable;
        // We don't own this so we don't free it
        ConstructorDecl* constructorDecl;

        Stmt* deepCopy() const override {
            auto result = new ConstructStructMemberVariableStmt(startPosition(), endPosition(),
                                                                refMemberVariable,
                                                                constructorDecl);
            return result;
        }

    };
}

#endif //GULC_CONSTRUCTSTRUCTMEMBERVARIABLESTMT_HPP
