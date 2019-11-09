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

#ifndef GULC_DESTRUCTMEMBERVARIABLEEXPR_HPP
#define GULC_DESTRUCTMEMBERVARIABLEEXPR_HPP

#include <AST/Decls/DestructorDecl.hpp>
#include "RefStructMemberVariableExpr.hpp"

namespace gulc {
    class DestructMemberVariableExpr : public Expr {
    public:
        static bool classof(const Expr *expr) { return expr->getExprKind() == Kind::DestructMemberVariable; }

        DestructMemberVariableExpr(TextPosition startPosition, TextPosition endPosition,
                                   RefStructMemberVariableExpr* memberVariable, DestructorDecl* destructor)
                : Expr(Kind::DestructMemberVariable, startPosition, endPosition),
                  memberVariable(memberVariable), destructor(destructor) {}

        RefStructMemberVariableExpr* memberVariable;
        // We don't own this so we don't delete it
        DestructorDecl* destructor;

        Expr* deepCopy() const override {
            auto result = new DestructMemberVariableExpr(startPosition(), endPosition(),
                                                        llvm::dyn_cast<RefStructMemberVariableExpr>(memberVariable->deepCopy()),
                                                        destructor);
            if (resultType) {
                result->resultType = resultType->deepCopy();
            }
            result->isUnreachable = isUnreachable;
            return result;
        }

        ~DestructMemberVariableExpr() override {
            delete memberVariable;
        }

    };
}

#endif //GULC_DESTRUCTMEMBERVARIABLEEXPR_HPP
