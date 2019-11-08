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

#ifndef GULC_DESTRUCTLOCALVARIABLEEXPR_HPP
#define GULC_DESTRUCTLOCALVARIABLEEXPR_HPP

#include <AST/Expr.hpp>
#include "RefLocalVariableExpr.hpp"

namespace gulc {
    class RefLocalVariableExpr;

    class DestructLocalVariableExpr : public Expr {
    public:
        static bool classof(const Expr *expr) { return expr->getExprKind() == Kind::DestructLocalVariable; }

        DestructLocalVariableExpr(TextPosition startPosition, TextPosition endPosition,
                                  RefLocalVariableExpr* localVariable, DestructorDecl* destructor)
                : Expr(Kind::DestructLocalVariable, startPosition, endPosition),
                  localVariable(localVariable), destructor(destructor) {}

        RefLocalVariableExpr* localVariable;
        // We don't own this so we don't delete it
        DestructorDecl* destructor;

        Expr* deepCopy() const override{
            auto result = new DestructLocalVariableExpr(startPosition(), endPosition(),
                                                        llvm::dyn_cast<RefLocalVariableExpr>(localVariable->deepCopy()),
                                                        destructor);
            if (resultType) {
                result->resultType = resultType->deepCopy();
            }
            return result;
        }

        ~DestructLocalVariableExpr() override {
            delete localVariable;
        }

    };
}

#endif //GULC_DESTRUCTLOCALVARIABLEEXPR_HPP
