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

#ifndef GULC_DESTRUCTPARAMETEREXPR_HPP
#define GULC_DESTRUCTPARAMETEREXPR_HPP

#include <AST/Expr.hpp>
#include <AST/Decls/DestructorDecl.hpp>
#include "RefParameterExpr.hpp"

namespace gulc {
    class DestructParameterExpr : public Expr {
    public:
        static bool classof(const Expr *expr) { return expr->getExprKind() == Kind::DestructParameter; }

        DestructParameterExpr(TextPosition startPosition, TextPosition endPosition,
                              RefParameterExpr* parameter, DestructorDecl* destructor)
                : Expr(Kind::DestructParameter, startPosition, endPosition),
                  parameter(parameter), destructor(destructor) {}

        RefParameterExpr* parameter;
        // We don't own this so we don't delete it
        DestructorDecl* destructor;

        Expr* deepCopy() const override{
            auto result = new DestructParameterExpr(startPosition(), endPosition(),
                                                        llvm::dyn_cast<RefParameterExpr>(parameter->deepCopy()),
                                                        destructor);
            if (resultType) {
                result->resultType = resultType->deepCopy();
            }
            result->isUnreachable = isUnreachable;
            return result;
        }

        ~DestructParameterExpr() override {
            delete parameter;
        }
    };
}

#endif //GULC_DESTRUCTPARAMETEREXPR_HPP