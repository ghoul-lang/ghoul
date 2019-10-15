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

#ifndef GULC_REFNAMESPACEFUNCTIONEXPR_HPP
#define GULC_REFNAMESPACEFUNCTIONEXPR_HPP

#include <AST/Expr.hpp>
#include <vector>

namespace gulc {
    class RefNamespaceFunctionExpr : public Expr {
    public:
        static bool classof(const Expr *expr) { return expr->getExprKind() == Kind::RefNamespaceFunction; }

        RefNamespaceFunctionExpr(TextPosition startPosition, TextPosition endPosition,
                                 std::string name, std::vector<Expr*> templateArguments, std::string mangledName)
                : Expr(Kind::RefNamespaceFunction, startPosition, endPosition),
                  templateArguments(std::move(templateArguments)), _name(std::move(name)),
                  _mangledName(std::move(mangledName)) {}

        std::vector<Expr*> templateArguments;
        std::string name() const { return _name; }
        std::string mangledName() const { return _mangledName; }

        bool hasTemplateArguments() const { return !templateArguments.empty(); }

        ~RefNamespaceFunctionExpr() override {
            for (Expr*& templateArgument : templateArguments) {
                delete templateArgument;
            }
        }

    private:
        std::string _name;
        std::string _mangledName;

    };
}

#endif //GULC_REFNAMESPACEFUNCTIONEXPR_HPP
