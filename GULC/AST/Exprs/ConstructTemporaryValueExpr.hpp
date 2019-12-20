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

#ifndef GULC_CONSTRUCTTEMPORARYVALUEEXPR_HPP
#define GULC_CONSTRUCTTEMPORARYVALUEEXPR_HPP

#include <AST/Expr.hpp>
#include <vector>
#include <AST/Decls/ConstructorDecl.hpp>

namespace gulc {
    /**
     * This is used to contain explicit constructor calls (i.e. `StructName(44, 22, expr)`)
     */
    class ConstructTemporaryValueExpr : public Expr {
    public:
        static bool classof(const Expr *expr) { return expr->getExprKind() == Kind::ConstructTemporaryValue; }

        ConstructTemporaryValueExpr(TextPosition startPosition, TextPosition endPosition,
                                    ConstructorDecl* constructorDecl, std::vector<Expr*> arguments)
                : Expr(Kind::ConstructTemporaryValue, startPosition, endPosition),
                  constructorDecl(constructorDecl), arguments(std::move(arguments)) {}

        ConstructorDecl* constructorDecl;
        std::vector<Expr*> arguments;
        bool hasArguments() const { return !arguments.empty(); }

        Expr* deepCopy() const override {
            std::vector<Expr*> copiedArguments;

            for (Expr* arg : arguments) {
                copiedArguments.push_back(arg->deepCopy());
            }

            auto result = new ConstructTemporaryValueExpr(startPosition(), endPosition(),
                                                          constructorDecl,
                                                          std::move(copiedArguments));
            if (resultType) {
                result->resultType = resultType->deepCopy();
            }
            result->isUnreachable = isUnreachable;
            return result;
        }

        ~ConstructTemporaryValueExpr() override {
            for (Expr* argument : arguments) {
                delete argument;
            }
        }

    };
}

#endif //GULC_CONSTRUCTTEMPORARYVALUEEXPR_HPP
