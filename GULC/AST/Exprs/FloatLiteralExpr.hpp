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

#ifndef GULC_FLOATLITERALEXPR_HPP
#define GULC_FLOATLITERALEXPR_HPP

#include <AST/Expr.hpp>
#include <string>

namespace gulc {
    class FloatLiteralExpr : public Expr {
    public:
        static bool classof(const Expr *expr) { return expr->getExprKind() == Kind::FloatLiteral; }

        FloatLiteralExpr(TextPosition startPosition, TextPosition endPosition, std::string numberValue)
                : Expr(Kind::FloatLiteral, startPosition, endPosition),
                  _numberValue(std::move(numberValue)) {}

        std::string numberValue() const { return _numberValue; }

        Expr* deepCopy() const override {
            return new FloatLiteralExpr(startPosition(), endPosition(),
                                        _numberValue);
        }

    private:
        std::string _numberValue;

    };
}

#endif //GULC_FLOATLITERALEXPR_HPP
