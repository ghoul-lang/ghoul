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

#ifndef GULC_LVALUETORVALUEEXPR_HPP
#define GULC_LVALUETORVALUEEXPR_HPP

#include <AST/Expr.hpp>

namespace gulc {
    class LValueToRValueExpr : public Expr {
    public:
        static bool classof(const Expr *expr) { return expr->getExprKind() == Kind::LValueToRValue; }

        LValueToRValueExpr(TextPosition startPosition, TextPosition endPosition, Expr* lvalue,
                           bool deletePointer = true)
                : Expr(Kind::LValueToRValue, startPosition, endPosition),
                  lvalue(lvalue), _deletePointer(deletePointer) {}

        Expr* lvalue;

        ~LValueToRValueExpr() override {
            if (_deletePointer) {
                delete lvalue;
            }
        }

    private:
        bool _deletePointer;

    };
}

#endif //GULC_LVALUETORVALUEEXPR_HPP
