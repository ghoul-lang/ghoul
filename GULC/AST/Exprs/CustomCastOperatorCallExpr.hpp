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

#ifndef GULC_CUSTOMCASTOPERATORCALLEXPR_HPP
#define GULC_CUSTOMCASTOPERATORCALLEXPR_HPP

#include <AST/Expr.hpp>
#include <AST/Decls/CastOperatorDecl.hpp>

namespace gulc {
    class CustomCastOperatorCallExpr : public Expr {
    public:
        static bool classof(const Expr *expr) { return expr->getExprKind() == Kind::CustomCastOperatorCall; }

        CustomCastOperatorCallExpr(TextPosition startPosition, TextPosition endPosition,
                                   bool isVTableCall, CastOperatorDecl* castOperatorDecl,
                                   Expr* castee)
                : Expr(Kind::CustomCastOperatorCall, startPosition, endPosition),
                  isVTableCall(isVTableCall), castOperatorDecl(castOperatorDecl),
                  castee(castee) {}

        // Tells us if we need to do a vtable lookup for the operator or not
        bool isVTableCall;
        // We don't own this so we don't free it
        CastOperatorDecl* castOperatorDecl;
        Expr* castee;

        Expr* deepCopy() const override {
            auto result = new CustomCastOperatorCallExpr(startPosition(), endPosition(),
                                                         isVTableCall, castOperatorDecl,
                                                         castee->deepCopy());
            if (resultType) {
                result->resultType = resultType->deepCopy();
            }
            result->isUnreachable = isUnreachable;
            return result;
        }

        ~CustomCastOperatorCallExpr() override {
            delete castee;
        }

    };
}

#endif //GULC_CUSTOMCASTOPERATORCALLEXPR_HPP
