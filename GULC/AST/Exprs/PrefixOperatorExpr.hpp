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

#ifndef GULC_PREFIXOPERATOREXPR_HPP
#define GULC_PREFIXOPERATOREXPR_HPP

#include <AST/Expr.hpp>
#include <string>

namespace gulc {
    class PrefixOperatorExpr : public Expr {
    public:
        static bool classof(const Expr *expr) { return expr->getExprKind() == Kind::PrefixOperator; }

        PrefixOperatorExpr(TextPosition startPosition, TextPosition endPosition,
                           std::string operatorName, Expr* expr)
                : Expr(Kind::PrefixOperator, startPosition, endPosition),
                  expr(expr), _operatorName(std::move(operatorName)) {}

        std::string operatorName() const { return _operatorName; }
        Expr* expr;

        ~PrefixOperatorExpr() override {
            delete expr;
        }

    private:
        std::string _operatorName;

    };
}

#endif //GULC_PREFIXOPERATOREXPR_HPP
