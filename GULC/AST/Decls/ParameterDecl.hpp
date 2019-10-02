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

#ifndef GULC_PARAMETERDECL_HPP
#define GULC_PARAMETERDECL_HPP

#include <AST/Decl.hpp>
#include <AST/Type.hpp>
#include <AST/Expr.hpp>

namespace gulc {
    class ParameterDecl : public Decl {
    public:
        static bool classof(const Decl *decl) { return decl->getDeclKind() == Kind::Parameter; }

        ParameterDecl(std::string name, std::string sourceFile, TextPosition startPosition, TextPosition endPosition,
                      Type* type, Expr* defaultArgument = nullptr)
                : Decl(Kind::Parameter, std::move(name), std::move(sourceFile), startPosition, endPosition),
                  type(type), _defaultArgument(defaultArgument) {}

        // TODO: Support 'Modifiers' and default modifiers like 'in' and 'out'
        Type* type;
        const Expr* defaultArgument() const { return _defaultArgument; }
        bool hasDefaultArgument() const { return _defaultArgument != nullptr; }

        ~ParameterDecl() override {
            delete type;
            delete _defaultArgument;
        }

    private:
        // TODO: Should this be 'ConstExpr'?
        Expr* _defaultArgument;

    };
}

#endif //GULC_PARAMETERDECL_HPP
