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

#ifndef GULC_REFFUNCTIONEXPR_HPP
#define GULC_REFFUNCTIONEXPR_HPP

#include <AST/Expr.hpp>
#include <vector>
#include <AST/Decls/FunctionDecl.hpp>

namespace gulc {
    class RefFunctionExpr : public Expr {
    public:
        static bool classof(const Expr *expr) { return expr->getExprKind() == Kind::RefFunction; }

        RefFunctionExpr(TextPosition startPosition, TextPosition endPosition, FunctionDecl* function)
                : Expr(Kind::RefFunction, startPosition, endPosition), _function(function) {}

        FunctionDecl* function() const { return _function; }

        Expr* deepCopy() const override {
            auto result = new RefFunctionExpr(startPosition(), endPosition(), _function);
            if (resultType) {
                result->resultType = resultType->deepCopy();
            }
            return result;
        }

    private:
        // We don't own this so we don't delete it...
        FunctionDecl* _function;

    };
}

#endif //GULC_REFFUNCTIONEXPR_HPP
