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

#ifndef GULC_MEMBERACCESSCALLEXPR_HPP
#define GULC_MEMBERACCESSCALLEXPR_HPP

#include <AST/Expr.hpp>
#include "IdentifierExpr.hpp"

namespace gulc {
    class MemberAccessCallExpr : public Expr {
    public:
        static bool classof(const Expr *expr) { return expr->getExprKind() == Kind::MemberAccessCall; }

        MemberAccessCallExpr(TextPosition startPosition, TextPosition endPosition,
                             bool isArrowCall, Expr* objectRef, IdentifierExpr* member)
                : Expr(Kind::MemberAccessCall, startPosition, endPosition),
                  objectRef(objectRef), member(member), _isArrowCall(isArrowCall) {}

        bool isArrowCall() const { return _isArrowCall; }
        Expr* objectRef;
        IdentifierExpr* member;

        ~MemberAccessCallExpr() override {
            delete objectRef;
            delete member;
        }

    private:
        bool _isArrowCall;

    };
}

#endif //GULC_MEMBERACCESSCALLEXPR_HPP
