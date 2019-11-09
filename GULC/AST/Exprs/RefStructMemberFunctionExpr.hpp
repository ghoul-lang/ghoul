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

#ifndef GULC_REFSTRUCTMEMBERFUNCTIONEXPR_HPP
#define GULC_REFSTRUCTMEMBERFUNCTIONEXPR_HPP

#include <AST/Expr.hpp>
#include <AST/Decls/FunctionDecl.hpp>
#include <AST/Types/StructType.hpp>

namespace gulc {
    class RefStructMemberFunctionExpr : public Expr {
    public:
        static bool classof(const Expr *expr) { return expr->getExprKind() == Kind::RefStructMemberFunction; }

        RefStructMemberFunctionExpr(TextPosition startPosition, TextPosition endPosition,
                                    Expr* objectRef,
                                    StructType* structType, FunctionDecl* refFunction)
                : Expr(Kind::RefStructMemberFunction, startPosition, endPosition),
                  objectRef(objectRef), structType(structType), refFunction(refFunction) {}

        Expr* objectRef;
        StructType* structType;
        FunctionDecl* refFunction;

        Expr* deepCopy() const override {
            auto result = new RefStructMemberFunctionExpr(startPosition(), endPosition(),
                                                          objectRef->deepCopy(),
                                                          llvm::dyn_cast<StructType>(structType->deepCopy()),
                                                          llvm::dyn_cast<FunctionDecl>(refFunction->deepCopy()));
            if (resultType) {
                result->resultType = resultType->deepCopy();
            }
            result->isUnreachable = isUnreachable;
            return result;
        }

        ~RefStructMemberFunctionExpr() override {
            delete objectRef;
            delete structType;
            delete refFunction;
        }

    };
}

#endif //GULC_REFSTRUCTMEMBERFUNCTIONEXPR_HPP
