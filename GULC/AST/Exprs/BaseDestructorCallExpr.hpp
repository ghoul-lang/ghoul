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

#ifndef GULC_BASEDESTRUCTORCALLEXPR_HPP
#define GULC_BASEDESTRUCTORCALLEXPR_HPP

#include <AST/Expr.hpp>
#include <vector>

namespace gulc {
    class DestructorDecl;

    class BaseDestructorCallExpr : public Expr {
    public:
        static bool classof(const Expr *expr) { return expr->getExprKind() == Kind::BaseDestructorCall; }

        BaseDestructorCallExpr(TextPosition startPosition, TextPosition endPosition,
                               DestructorDecl* baseDestructor)
                : Expr(Kind::BaseDestructorCall, startPosition, endPosition),
                  baseDestructor(baseDestructor) {}

        // We don't own this so we don't free it
        DestructorDecl* baseDestructor;

        Expr* deepCopy() const override {
            auto result = new BaseDestructorCallExpr(startPosition(), endPosition(),
                                                     baseDestructor);
            if (resultType) {
                result->resultType = resultType->deepCopy();
            }
            return result;
        }

    };
}

#endif //GULC_BASEDESTRUCTORCALLEXPR_HPP
