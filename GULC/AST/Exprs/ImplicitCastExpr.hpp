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

#ifndef GULC_IMPLICITCASTEXPR_HPP
#define GULC_IMPLICITCASTEXPR_HPP

#include <AST/Expr.hpp>
#include <AST/Type.hpp>

namespace gulc {
    class ImplicitCastExpr : public Expr {
    public:
        static bool classof(const Expr *expr) { return expr->getExprKind() == Kind::ImplicitCast; }

        ImplicitCastExpr(TextPosition startPosition, TextPosition endPosition,
                         Type* castType, Expr* castee)
                : Expr(Kind::ImplicitCast, startPosition, endPosition),
                  castType(castType), castee(castee) {}

        Type* castType;
        Expr* castee;

        Expr* deepCopy() const override {
            auto result = new ImplicitCastExpr(startPosition(), endPosition(),
                                               castType->deepCopy(), castee->deepCopy());
            if (resultType) {
                result->resultType = resultType->deepCopy();
            }
            result->isUnreachable = isUnreachable;
            return result;
        }

        ~ImplicitCastExpr() override {
            delete castType;
            delete castee;
        }

    };
}

#endif //GULC_IMPLICITCASTEXPR_HPP
