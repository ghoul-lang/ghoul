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

#ifndef GULC_REFGLOBALVARIABLEEXPR_HPP
#define GULC_REFGLOBALVARIABLEEXPR_HPP

#include <AST/Expr.hpp>
#include <AST/Decls/GlobalVariableDecl.hpp>

namespace gulc {
    class RefGlobalVariableExpr : public Expr {
    public:
        static bool classof(const Expr *expr) { return expr->getExprKind() == Kind::RefGlobalVariable; }

        RefGlobalVariableExpr(TextPosition startPosition, TextPosition endPosition,
                              GlobalVariableDecl* globalVariable)
                : Expr(Kind::RefGlobalVariable, startPosition, endPosition),
                  _globalVariable(globalVariable) {}

        GlobalVariableDecl* globalVariable() const { return _globalVariable; }

        Expr* deepCopy() const override {
            auto result = new RefGlobalVariableExpr(startPosition(), endPosition(),
                                                    _globalVariable);
            if (resultType) {
                result->resultType = resultType->deepCopy();
            }
            return result;
        }

    private:
        GlobalVariableDecl* _globalVariable;

    };
}

#endif //GULC_REFGLOBALVARIABLEEXPR_HPP
