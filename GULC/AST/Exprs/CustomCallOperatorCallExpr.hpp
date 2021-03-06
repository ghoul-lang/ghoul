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

#ifndef GULC_CUSTOMCALLOPERATORCALLEXPR_HPP
#define GULC_CUSTOMCALLOPERATORCALLEXPR_HPP

#include <AST/Expr.hpp>
#include <AST/Decls/CallOperatorDecl.hpp>

namespace gulc {
    class CustomCallOperatorCallExpr : public Expr {
    public:
        static bool classof(const Expr *expr) { return expr->getExprKind() == Kind::CustomCallOperatorCall; }

        CustomCallOperatorCallExpr(TextPosition startPosition, TextPosition endPosition,
                                   bool isVTableCall, CallOperatorDecl* callOperatorDecl,
                                   Expr* objectRef, std::vector<Expr*> arguments)
                : Expr(Kind::CustomCallOperatorCall, startPosition, endPosition),
                  isVTableCall(isVTableCall), callOperatorDecl(callOperatorDecl),
                  objectRef(objectRef), arguments(std::move(arguments)) {}

        // Tells us if we need to do a vtable lookup for the operator or not
        bool isVTableCall;
        // We don't own this so we don't free it
        CallOperatorDecl* callOperatorDecl;
        Expr* objectRef;
        std::vector<Expr*> arguments;

        Expr* deepCopy() const override {
            std::vector<Expr*> copiedArguments;

            for (Expr* argument : arguments) {
                copiedArguments.push_back(argument->deepCopy());
            }

            auto result = new CustomCallOperatorCallExpr(startPosition(), endPosition(),
                                                          isVTableCall, callOperatorDecl,
                                                          objectRef->deepCopy(), std::move(copiedArguments));
            if (resultType) {
                result->resultType = resultType->deepCopy();
            }
            result->isUnreachable = isUnreachable;
            return result;
        }

        ~CustomCallOperatorCallExpr() override {
            delete objectRef;

            for (Expr* argument : arguments) {
                delete argument;
            }
        }

    };
}

#endif //GULC_CUSTOMCALLOPERATORCALLEXPR_HPP
