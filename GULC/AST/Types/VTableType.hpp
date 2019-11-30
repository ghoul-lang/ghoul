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

#ifndef GULC_VTABLETYPE_HPP
#define GULC_VTABLETYPE_HPP

#include <AST/Type.hpp>

namespace gulc {
    /// Special type dedicated solely for use on hidden vtable members
    class VTableType : public Type {
    public:
        static bool classof(const Type *expr) { return expr->getTypeKind() == Kind::VTable; }

        // NOTE: vtable is always const. They are NOT modifiable in GUL
        VTableType(TextPosition startPosition, TextPosition endPosition)
                : Type(Kind::VTable, startPosition, endPosition, TypeQualifier::Const) {}

        std::string getString() const override { return "#vtable#"; }

        Type* deepCopy() const override {
            return new VTableType(startPosition(), endPosition());
        }

    };
}

#endif //GULC_VTABLETYPE_HPP
