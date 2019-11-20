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

#ifndef GULC_REFBASEEXPR_HPP
#define GULC_REFBASEEXPR_HPP

#include <AST/Expr.hpp>
#include <AST/Types/StructType.hpp>

namespace gulc {
    /**
     * This is used in scenarios where you might call `base.i` or `base.baseFunction(...)`. It is a special case that
     * is NOT just casting `this` to the base type. `base` allows you to call ALL base class functions as non-virtual
     * functions. It also allows you to call shadowed members more easily (rather than casting `this` to the base type)
     *
     * NOTE: Because `base` != `(BaseType&)this` you CANNOT use `base` in any situation where you would be passing
     *       `base`. `base` is NOT an lvalue. `base` CANNOT be assigned to (but base members can be).
     */
    class RefBaseExpr : public Expr {
    public:
        static bool classof(const Expr *expr) { return expr->getExprKind() == Kind::RefBase; }

        RefBaseExpr(TextPosition startPosition, TextPosition endPosition,
                    Expr* refThis)
                : Expr(Kind::RefBase, startPosition, endPosition),
                  refThis(refThis) {}

        Expr* refThis;

        Expr* deepCopy() const override {
            auto result = new RefBaseExpr(startPosition(), endPosition(),
                                          refThis->deepCopy());
            if (resultType) {
                result->resultType = resultType->deepCopy();
            }
            result->isUnreachable = isUnreachable;
            return result;
        }

        ~RefBaseExpr() override {
            delete refThis;
        }

    };
}

#endif //GULC_REFBASEEXPR_HPP
