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

#ifndef GULC_REFLOCALVARIABLEEXPR_HPP
#define GULC_REFLOCALVARIABLEEXPR_HPP

#include <AST/Expr.hpp>

namespace gulc {
    class RefLocalVariableExpr : public Expr {
    public:
        static bool classof(const Expr *expr) { return expr->getExprKind() == Kind::RefLocalVariable; }

        RefLocalVariableExpr(TextPosition startPosition, TextPosition endPosition, std::string name)
                : Expr(Kind::RefLocalVariable, startPosition, endPosition),
                  _name(std::move(name)) {}

        std::string name() const { return _name; }

        Expr* deepCopy() const override {
            auto result = new RefLocalVariableExpr(startPosition(), endPosition(), _name);
            if (resultType) {
                result->resultType = resultType->deepCopy();
            }
            result->isUnreachable = isUnreachable;
            return result;
        }

    private:
        std::string _name;

    };
}

#endif //GULC_REFLOCALVARIABLEEXPR_HPP
