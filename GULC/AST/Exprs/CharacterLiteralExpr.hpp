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

#ifndef GULC_CHARACTERLITERALEXPR_HPP
#define GULC_CHARACTERLITERALEXPR_HPP

#include <AST/Expr.hpp>

namespace gulc {
    class CharacterLiteralExpr : public Expr {
    public:
        static bool classof(const Expr *expr) { return expr->getExprKind() == Kind::CharacterLiteral; }

        CharacterLiteralExpr(TextPosition startPosition, TextPosition endPosition, unsigned int characterLiteral)
                : Expr(Kind::CharacterLiteral, startPosition, endPosition),
                  _characterValue(characterLiteral) {}

        unsigned int characterValue() const { return _characterValue; }

        Expr* deepCopy() const override {
            auto result = new CharacterLiteralExpr(startPosition(), endPosition(),
                                                   _characterValue);
            if (resultType) {
                result->resultType = resultType->deepCopy();
            }
            return result;
        }

    private:
        unsigned int _characterValue;

    };
}

#endif //GULC_CHARACTERLITERALEXPR_HPP
