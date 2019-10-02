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

#ifndef GULC_STRINGLITERALEXPR_HPP
#define GULC_STRINGLITERALEXPR_HPP

#include <AST/Expr.hpp>
#include <string>

namespace gulc {
    class StringLiteralExpr : public Expr {
    public:
        static bool classof(const Expr *expr) { return expr->getExprKind() == Kind::StringLiteral; }

        StringLiteralExpr(TextPosition startPosition, TextPosition endPosition, std::string stringValue)
                : Expr(Kind::StringLiteral, startPosition, endPosition),
                  _stringValue(std::move(stringValue)) {}

        std::string stringValue() const { return _stringValue; }

    private:
        std::string _stringValue;

    };
}

#endif //GULC_STRINGLITERALEXPR_HPP
