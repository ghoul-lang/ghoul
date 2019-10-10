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

namespace gulc {
    class EnumType : public Type {
    public:
        static bool classof(const Type *expr) { return expr->getTypeKind() == Kind::Enum; }

        EnumType(TextPosition startPosition, TextPosition endPosition,
                 std::string name, Type* baseType)
                : Type(Kind::Enum, startPosition, endPosition),
                  _name(std::move(name)), _baseType(baseType) {}

        std::string name() const { return _name; }
        Type* baseType() const { return _baseType; }
        
        std::string getString() const override { return _name; }

        ~EnumType() override {
            delete _baseType;
        }

    private:
        std::string _name;
        Type* _baseType;

    };
}

#endif //GULC_ENUMTYPE_HPP
