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

#ifndef GULC_LOCALVARIABLEDECLEXPR_HPP
#define GULC_LOCALVARIABLEDECLEXPR_HPP

#include <AST/Expr.hpp>
#include <string>
#include <vector>
#include <AST/Decls/ConstructorDecl.hpp>

namespace gulc {
    class LocalVariableDeclExpr : public Expr {
    public:
        static bool classof(const Expr *expr) { return expr->getExprKind() == Kind::LocalVariableDecl; }

        LocalVariableDeclExpr(TextPosition startPosition, TextPosition endPosition,
                              Expr* type, std::string name)
                : Expr(Kind::LocalVariableDecl, startPosition, endPosition),
                  type(type), foundConstructor(nullptr), _name(std::move(name)) {}

        Expr* type;
        std::string name() const { return _name; }
        std::vector<Expr*> initializerArgs;
        bool hasInitializer() const { return !initializerArgs.empty(); }

        // NOTE: We don't own this pointer so we don't free it
        ConstructorDecl* foundConstructor;

        Expr* deepCopy() const override {
            std::vector<Expr*> copiedInitializerArgs{};

            for (Expr* initializerArg : initializerArgs) {
                copiedInitializerArgs.push_back(initializerArg->deepCopy());
            }

            auto result = new LocalVariableDeclExpr(startPosition(), endPosition(),
                                                    type->deepCopy(), name());
            result->initializerArgs = std::move(copiedInitializerArgs);
            result->foundConstructor = foundConstructor;
            if (resultType) {
                result->resultType = resultType->deepCopy();
            }
            result->isUnreachable = isUnreachable;
            return result;
        }

        ~LocalVariableDeclExpr() override {
            for (Expr* initializerArg : initializerArgs) {
                delete initializerArg;
            }

            delete type;
        }

    private:
        std::string _name;

    };
}

#endif //GULC_LOCALVARIABLEDECLEXPR_HPP
