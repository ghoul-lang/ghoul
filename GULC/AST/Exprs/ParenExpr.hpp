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

#ifndef GULC_PARENEXPR_HPP
#define GULC_PARENEXPR_HPP

#include <AST/Expr.hpp>

namespace gulc {
    class ParenExpr : public Expr {
    public:
        static bool classof(const Expr *expr) { return expr->getExprKind() == Kind::Paren; }

        ParenExpr(TextPosition startPosition, TextPosition endPosition, Expr* containedExpr)
                : Expr(Kind::Paren, startPosition, endPosition),
                  containedExpr(containedExpr) {}

        Expr* containedExpr;

        ~ParenExpr() override {
            delete containedExpr;
        }

    private:

    };
}

#endif //GULC_PARENEXPR_HPP
