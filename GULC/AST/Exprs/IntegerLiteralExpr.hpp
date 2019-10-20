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

#ifndef GULC_INTEGERLITERALEXPR_HPP
#define GULC_INTEGERLITERALEXPR_HPP

#include <AST/Expr.hpp>
#include <string>

namespace gulc {
    class IntegerLiteralExpr : public Expr {
    public:
        static bool classof(const Expr *expr) { return expr->getExprKind() == Kind::IntegerLiteral; }

        IntegerLiteralExpr(TextPosition startPosition, TextPosition endPosition,
                           unsigned int numberBase, std::string numberString)
                : Expr(Kind::IntegerLiteral, startPosition, endPosition),
                  numberString(std::move(numberString)), _numberBase(numberBase) {}

        unsigned int numberBase() const { return _numberBase; }
        std::string numberString;

        Expr* deepCopy() const override {
            auto result = new IntegerLiteralExpr(startPosition(), endPosition(),
                                                 _numberBase, numberString);
            if (resultType) {
                result->resultType = resultType->deepCopy();
            }
            return result;
        }

    private:
        unsigned int _numberBase;

    };
}

#endif //GULC_INTEGERLITERALEXPR_HPP
