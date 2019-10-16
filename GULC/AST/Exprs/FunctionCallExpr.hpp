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

#ifndef GULC_FUNCTIONCALLEXPR_HPP
#define GULC_FUNCTIONCALLEXPR_HPP

#include <AST/Expr.hpp>
#include <vector>

namespace gulc {
    class FunctionCallExpr : public Expr {
    public:
        static bool classof(const Expr *expr) { return expr->getExprKind() == Kind::FunctionCall; }

        FunctionCallExpr(TextPosition startPosition, TextPosition endPosition,
                         Expr* functionReference, std::vector<Expr*> arguments)
                : Expr(Kind::FunctionCall, startPosition, endPosition),
                  functionReference(functionReference), arguments(std::move(arguments)) {}

        Expr* functionReference;
        std::vector<Expr*> arguments;
        bool hasArguments() const { return !arguments.empty(); }

        Expr* deepCopy() const override {
            std::vector<Expr*> copiedArguments;

            for (Expr* arg : arguments) {
                copiedArguments.push_back(arg->deepCopy());
            }

            return new FunctionCallExpr(startPosition(), endPosition(),
                                        functionReference, std::move(copiedArguments));
        }

        ~FunctionCallExpr() override {
            delete functionReference;

            for (Expr* argument : arguments) {
                delete argument;
            }
        }

    };
}

#endif //GULC_FUNCTIONCALLEXPR_HPP
