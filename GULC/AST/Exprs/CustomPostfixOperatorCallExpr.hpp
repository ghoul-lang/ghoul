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

#ifndef GULC_CUSTOMPOSTFIXOPERATORCALLEXPR_HPP
#define GULC_CUSTOMPOSTFIXOPERATORCALLEXPR_HPP

#include <AST/Expr.hpp>
#include <AST/Decls/OperatorDecl.hpp>

namespace gulc {
    class CustomPostfixOperatorCallExpr : public Expr {
    public:
        static bool classof(const Expr *expr) { return expr->getExprKind() == Kind::CustomPostfixOperatorCall; }

        CustomPostfixOperatorCallExpr(TextPosition startPosition, TextPosition endPosition,
                                      bool isVTableCall, OperatorDecl* operatorDecl,
                                      Expr *expr)
                : Expr(Kind::CustomPostfixOperatorCall, startPosition, endPosition),
                  isVTableCall(isVTableCall), operatorDecl(operatorDecl), expr(expr) {}

        // Tells us if we need to do a vtable lookup for the operator or not
        bool isVTableCall;
        // We don't own this so we don't free it
        OperatorDecl *operatorDecl;
        Expr *expr;

        Expr *deepCopy() const override {
            auto result = new CustomPostfixOperatorCallExpr(startPosition(), endPosition(),
                                                            isVTableCall, operatorDecl,
                                                            expr->deepCopy());
            if (resultType) {
                result->resultType = resultType->deepCopy();
            }
            result->isUnreachable = isUnreachable;
            return result;
        }

        ~CustomPostfixOperatorCallExpr() override {
            delete expr;
        }

    };
}

#endif //GULC_CUSTOMPOSTFIXOPERATORCALLEXPR_HPP
