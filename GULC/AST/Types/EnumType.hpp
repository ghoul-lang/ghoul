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

#ifndef GULC_ENUMTYPE_HPP
#define GULC_ENUMTYPE_HPP

#include <AST/Type.hpp>
#include <AST/Decls/NamespaceDecl.hpp>
#include <AST/Decls/EnumDecl.hpp>

namespace gulc {
    class EnumType : public Type {
    public:
        static bool classof(const Type *expr) { return expr->getTypeKind() == Kind::Enum; }

        EnumType(TextPosition startPosition, TextPosition endPosition,
                 std::string name, Type* baseType, EnumDecl* decl)
                : EnumType(startPosition, endPosition, std::move(name), baseType, decl, nullptr) {}

        EnumType(TextPosition startPosition, TextPosition endPosition,
                 std::string name, Type* baseType, EnumDecl* decl, NamespaceDecl* owningPrototype)
                : Type(Kind::Enum, startPosition, endPosition),
                  _name(std::move(name)), _baseType(baseType), _decl(decl), _owningPrototype(owningPrototype) {}

        std::string name() const { return _name; }
        Type* baseType() const { return _baseType; }
        
        std::string getString() const override { return _name; }

        EnumDecl* decl() const { return _decl; }
        NamespaceDecl* owningPrototype() const { return _owningPrototype; }

        ~EnumType() override {
            // We don't own `_decl` so we don't delete it
            // We don't own `_owningPrototype` so we don't delete it
            delete _baseType;
        }

    private:
        std::string _name;
        Type* _baseType;
        EnumDecl* _decl;
        NamespaceDecl* _owningPrototype;

    };
}

#endif //GULC_ENUMTYPE_HPP
