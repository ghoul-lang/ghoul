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

#ifndef GULC_REFSTRUCTMEMBERVARIABLEEXPR_HPP
#define GULC_REFSTRUCTMEMBERVARIABLEEXPR_HPP

#include <AST/Expr.hpp>
#include <AST/Decls/GlobalVariableDecl.hpp>
#include <AST/Types/StructType.hpp>

namespace gulc {
    class RefStructMemberVariableExpr : public Expr {
    public:
        static bool classof(const Expr *expr) { return expr->getExprKind() == Kind::RefStructMemberVariable; }

        RefStructMemberVariableExpr(TextPosition startPosition, TextPosition endPosition,
                                    Expr* objectRef,
                                    StructType* structType, GlobalVariableDecl* refVariable)
                : Expr(Kind::RefStructMemberVariable, startPosition, endPosition),
                  objectRef(objectRef), structType(structType), refVariable(refVariable) {}

        // objectRef is the actual variable that is a struct. This could be a `RefGlobalVariableExpr`, a function call, anything.
        Expr* objectRef;
        StructType* structType;
        GlobalVariableDecl* refVariable;

        Expr* deepCopy() const override {
            auto result = new RefStructMemberVariableExpr(startPosition(), endPosition(),
                                                          objectRef->deepCopy(),
                                                          llvm::dyn_cast<StructType>(structType->deepCopy()),
                                                          llvm::dyn_cast<GlobalVariableDecl>(refVariable->deepCopy()));
            if (resultType) {
                result->resultType = resultType->deepCopy();
            }
            return result;
        }

        ~RefStructMemberVariableExpr() override {
            delete objectRef;
            delete structType;
            delete refVariable;
        }

    };
}

#endif //GULC_REFSTRUCTMEMBERVARIABLEEXPR_HPP
