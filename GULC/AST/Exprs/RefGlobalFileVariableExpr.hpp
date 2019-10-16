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

#ifndef GULC_REFGLOBALFILEVARIABLEEXPR_HPP
#define GULC_REFGLOBALFILEVARIABLEEXPR_HPP

#include <AST/Expr.hpp>

namespace gulc {
    class RefGlobalFileVariableExpr : public Expr {
    public:
        static bool classof(const Expr *expr) { return expr->getExprKind() == Kind::RefGlobalFileVariable; }

        RefGlobalFileVariableExpr(TextPosition startPosition, TextPosition endPosition,
                                  std::string name, std::string mangledName)
                : Expr(Kind::RefGlobalFileVariable, startPosition, endPosition),
                  _name(std::move(name)), _mangledName(std::move(mangledName)) {}

        std::string name() const { return _name; }
        std::string mangledName() const { return _mangledName; }

        Expr* deepCopy() const override {
            return new RefGlobalFileVariableExpr(startPosition(), endPosition(),
                                                 _name, _mangledName);
        }

    private:
        std::string _name;
        std::string _mangledName;

    };
}

#endif //GULC_REFGLOBALFILEVARIABLEEXPR_HPP
