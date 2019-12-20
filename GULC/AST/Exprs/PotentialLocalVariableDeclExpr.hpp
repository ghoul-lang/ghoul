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

#ifndef GULC_POTENTIALLOCALVARIABLEDECLEXPR_HPP
#define GULC_POTENTIALLOCALVARIABLEDECLEXPR_HPP

#include <AST/Expr.hpp>
#include <vector>

namespace gulc {
    class PotentialLocalVariableDeclExpr : public Expr {
    public:
        static bool classof(const Expr *expr) { return expr->getExprKind() == Kind::PotentialLocalVariableDecl; }

        PotentialLocalVariableDeclExpr(TextPosition startPosition, TextPosition endPosition,
                                       Expr* type, std::string name, std::vector<Expr*> initializerArguments)
                : Expr(Kind::PotentialLocalVariableDecl, startPosition, endPosition),
                  type(type), name(std::move(name)), initializerArguments(std::move(initializerArguments)) {}

        Expr* type;
        std::string name;
        std::vector<Expr*> initializerArguments;

        Expr* deepCopy() const override {
            std::vector<Expr*> copiedInitializerArguments;

            for (Expr* initializerArgument : initializerArguments) {
                copiedInitializerArguments.push_back(initializerArgument->deepCopy());
            }

            auto result = new PotentialLocalVariableDeclExpr(startPosition(), endPosition(),
                                                             type->deepCopy(),
                                                             name, copiedInitializerArguments);
            if (resultType) {
                result->resultType = resultType->deepCopy();
            }
            result->isUnreachable = isUnreachable;
            return result;
        }

        ~PotentialLocalVariableDeclExpr() override {
            delete type;
        }

    };
}

#endif //GULC_POTENTIALLOCALVARIABLEDECLEXPR_HPP
