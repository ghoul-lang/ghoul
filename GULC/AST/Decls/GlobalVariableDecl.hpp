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

#ifndef GULC_GLOBALVARIABLEDECL_HPP
#define GULC_GLOBALVARIABLEDECL_HPP

#include <AST/Decl.hpp>
#include <AST/Type.hpp>
#include <AST/Expr.hpp>

namespace gulc {
    class GlobalVariableDecl : public Decl {
    public:
        static bool classof(const Decl *decl) { return decl->getDeclKind() == Kind::GlobalVariable; }

        GlobalVariableDecl(std::vector<Attr*> attributes, std::string name, std::string sourceFile,
                           TextPosition startPosition, TextPosition endPosition, Visibility visibility,
                           Type* type, Expr* initialValue = nullptr)
                : Decl(Kind::GlobalVariable, std::move(attributes), std::move(name), std::move(sourceFile),
                       startPosition, endPosition, visibility),
                  type(type), initialValue(initialValue) {}

        Type* type;
        Expr* initialValue;

        bool hasInitialValue() const { return initialValue != nullptr; }

        Decl* deepCopy() const override {
            std::vector<Attr*> copiedAttributes;

            for (Attr* attribute : _attributes) {
                copiedAttributes.push_back(attribute->deepCopy());
            }

            auto result = new GlobalVariableDecl(copiedAttributes, name(), sourceFile(),
                                                 startPosition(), endPosition(),
                                                 visibility(),
                                                 type->deepCopy(),
                                                 // Using the safe navigation operator `?.` would be great...
                                                 initialValue ? initialValue->deepCopy() : nullptr);
            result->parentNamespace = parentNamespace;
            result->parentStruct = parentStruct;
            return result;
        }

        ~GlobalVariableDecl() override {
            delete type;
            delete initialValue;
        }

    };
}

#endif //GULC_GLOBALVARIABLEDECL_HPP
