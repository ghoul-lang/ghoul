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

#ifndef GULC_CUSTOMINFIXOPERATORCALLEXPR_HPP
#define GULC_CUSTOMINFIXOPERATORCALLEXPR_HPP

#include <AST/Expr.hpp>
#include <AST/Decls/OperatorDecl.hpp>

namespace gulc {
    class CustomInfixOperatorCallExpr : public Expr {
    public:
        static bool classof(const Expr *expr) { return expr->getExprKind() == Kind::CustomInfixOperatorCall; }

        CustomInfixOperatorCallExpr(TextPosition startPosition, TextPosition endPosition,
                                    bool isVTableCall, OperatorDecl* operatorDecl,
                                    Expr* leftValue, Expr* rightValue)
                : Expr(Kind::CustomInfixOperatorCall, startPosition, endPosition),
                  isVTableCall(isVTableCall), operatorDecl(operatorDecl),
                  leftValue(leftValue), rightValue(rightValue) {}

        // Tells us if we need to do a vtable lookup for the operator or not
        bool isVTableCall;
        // We don't own this so we don't free it
        OperatorDecl* operatorDecl;
        Expr* leftValue;
        Expr* rightValue;

        Expr* deepCopy() const override {
            auto result = new CustomInfixOperatorCallExpr(startPosition(), endPosition(),
                                                          isVTableCall, operatorDecl,
                                                          leftValue->deepCopy(), rightValue->deepCopy());
            if (resultType) {
                result->resultType = resultType->deepCopy();
            }
            result->isUnreachable = isUnreachable;
            return result;
        }

        ~CustomInfixOperatorCallExpr() override {
            delete leftValue;
            delete rightValue;
        }

    };
}

#endif //GULC_CUSTOMINFIXOPERATORCALLEXPR_HPP
