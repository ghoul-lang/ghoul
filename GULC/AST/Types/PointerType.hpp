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

#ifndef GULC_POINTERTYPE_HPP
#define GULC_POINTERTYPE_HPP

#include <AST/Type.hpp>

namespace gulc {
    class PointerType : public Type {
    public:
        static bool classof(const Type *expr) { return expr->getTypeKind() == Kind::Pointer; }

        PointerType(TextPosition startPosition, TextPosition endPosition, TypeQualifier qualifier,
                    Type* pointToType)
                : Type(Kind::Pointer, startPosition, endPosition, qualifier),
                  pointToType(pointToType) {}

        Type* pointToType;
        std::string getString() const override { return pointToType->getString() + "*"; }

        Type* deepCopy() const override {
            return new PointerType(startPosition(), endPosition(), qualifier(),
                                   pointToType->deepCopy());
        }

        ~PointerType() override {
            delete pointToType;
        }

    };
}

#endif //GULC_POINTERTYPE_HPP
