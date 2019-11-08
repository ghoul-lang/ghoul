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

#ifndef GULC_TEMPLATEPARAMETERDECL_HPP
#define GULC_TEMPLATEPARAMETERDECL_HPP

#include <AST/Decl.hpp>
#include <AST/Type.hpp>
#include <AST/Expr.hpp>

namespace gulc {
    class TemplateParameterDecl : public Decl {
    public:
        static bool classof(const Decl *decl) { return decl->getDeclKind() == Kind::TemplateParameter; }

        TemplateParameterDecl(std::string name, std::string sourceFile, TextPosition startPosition, TextPosition endPosition,
                              Type* type, Expr* defaultArgument = nullptr)
                : Decl(Kind::TemplateParameter, std::move(name), std::move(sourceFile), startPosition, endPosition),
                  type(type), _defaultArgument(defaultArgument) {}

        Type* type;
        const Expr* defaultArgument() const { return _defaultArgument; }
        bool hasDefaultArgument() const { return _defaultArgument != nullptr; }

        Decl* deepCopy() const override {
            auto result = new TemplateParameterDecl(name(), sourceFile(),
                                                    startPosition(), endPosition(),
                                                    type->deepCopy(),
                                                    _defaultArgument ? _defaultArgument->deepCopy() : nullptr);
            result->parentNamespace = parentNamespace;
            result->parentStruct = parentStruct;
            return result;
        }

        ~TemplateParameterDecl() override {
            delete type;
            delete _defaultArgument;
        }

    private:
        // TODO: Should this be 'ConstExpr'?
        Expr* _defaultArgument;

    };
}

#endif //GULC_TEMPLATEPARAMETERDECL_HPP
