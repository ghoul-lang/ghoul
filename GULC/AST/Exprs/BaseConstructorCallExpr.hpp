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

#ifndef GULC_BASECONSTRUCTORCALLEXPR_HPP
#define GULC_BASECONSTRUCTORCALLEXPR_HPP

#include <AST/Expr.hpp>
#include <vector>

namespace gulc {
    class ConstructorDecl;

    class BaseConstructorCallExpr : public Expr {
    public:
        static bool classof(const Expr *expr) { return expr->getExprKind() == Kind::BaseConstructorCall; }

        BaseConstructorCallExpr(TextPosition startPosition, TextPosition endPosition,
                                bool isThisCall, ConstructorDecl* baseConstructor, std::vector<Expr*> arguments)
                : Expr(Kind::BaseConstructorCall, startPosition, endPosition),
                  _isThisCall(isThisCall), baseConstructor(baseConstructor), arguments(std::move(arguments)) {}

        /// Specifies if the call was a `: this(...)` call instead of a `: base(...)` call
        bool isThisCall() const { return _isThisCall; }
        // We don't own this so we don't free it
        ConstructorDecl* baseConstructor;
        std::vector<Expr*> arguments;
        bool hasArguments() const { return !arguments.empty(); }

        Expr* deepCopy() const override {
            std::vector<Expr*> copiedArguments;

            for (Expr* arg : arguments) {
                copiedArguments.push_back(arg->deepCopy());
            }

            auto result = new BaseConstructorCallExpr(startPosition(), endPosition(),
                                                      _isThisCall,
                                                      baseConstructor, std::move(copiedArguments));
            if (resultType) {
                result->resultType = resultType->deepCopy();
            }
            return result;
        }

        ~BaseConstructorCallExpr() override {
            for (Expr* argument : arguments) {
                delete argument;
            }
        }

    private:
        bool _isThisCall;

    };
}

#endif //GULC_BASECONSTRUCTORCALLEXPR_HPP
