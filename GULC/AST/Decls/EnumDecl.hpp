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

#ifndef GULC_ENUMDECL_HPP
#define GULC_ENUMDECL_HPP

#include <AST/Decl.hpp>
#include <AST/Type.hpp>
#include <vector>
#include "EnumConstantDecl.hpp"

namespace gulc {
    class EnumDecl : public Decl {
    public:
        static bool classof(const Decl *decl) { return decl->getDeclKind() == Kind::Enum; }

        EnumDecl(std::vector<Attr*> attributes, std::string name, std::string sourceFile,
                 TextPosition startPosition, TextPosition endPosition, Visibility visibility, Type* baseType,
                 std::vector<EnumConstantDecl*> enumConstants)
                : Decl(Kind::Enum, std::move(attributes), std::move(name), std::move(sourceFile),
                       startPosition, endPosition, visibility),
                  baseType(baseType), _enumConstants(std::move(enumConstants)) {}

        Type* baseType;
        bool hasBaseType() const { return baseType != nullptr; }
        const std::vector<EnumConstantDecl*>& enumConstants() const { return _enumConstants; }

        bool hasConstants() const { return !_enumConstants.empty(); }

        Decl* deepCopy() const override {
            std::vector<Attr*> copiedAttributes;
            std::vector<EnumConstantDecl*> copiedConstants{};
            copiedConstants.reserve(_enumConstants.size());

            for (Attr* attribute : _attributes) {
                copiedAttributes.push_back(attribute->deepCopy());
            }

            for (EnumConstantDecl* enumConstantDecl : _enumConstants) {;
                // `deepCopy` for `EnumConstantDecl` is guaranteed to return `EnumConstantDecl*`
                copiedConstants.push_back(static_cast<EnumConstantDecl*>(enumConstantDecl->deepCopy()));
            }

            auto result = new EnumDecl(copiedAttributes, name(), sourceFile(),
                                       startPosition(), endPosition(),
                                       visibility(),
                                       baseType->deepCopy(), std::move(copiedConstants));
            result->parentNamespace = parentNamespace;
            result->parentStruct = parentStruct;
            return result;
        }

        ~EnumDecl() override {
            delete baseType;

            for (EnumConstantDecl* enumConstantDecl : _enumConstants) {
                delete enumConstantDecl;
            }
        }

    private:
        std::vector<EnumConstantDecl*> _enumConstants;

    };
}

#endif //GULC_ENUMDECL_HPP
