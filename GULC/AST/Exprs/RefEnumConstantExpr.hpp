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

#ifndef GULC_REFENUMCONSTANTEXPR_HPP
#define GULC_REFENUMCONSTANTEXPR_HPP

#include <AST/Expr.hpp>

namespace gulc {
    class RefEnumConstantExpr : public Expr {
    public:
        static bool classof(const Expr *expr) { return expr->getExprKind() == Kind::RefEnumConstant; }

        RefEnumConstantExpr(TextPosition startPosition, TextPosition endPosition,
                            std::string enumName, std::string constantName)
                : Expr(Kind::RefEnumConstant, startPosition, endPosition),
                  _enumName(std::move(enumName)), _constantName(std::move(constantName)) {}

        std::string enumName() const { return _enumName; }
        std::string constantName() const { return _constantName; }

    private:
        std::string _enumName;
        std::string _constantName;

    };
}

#endif //GULC_REFENUMCONSTANTEXPR_HPP
