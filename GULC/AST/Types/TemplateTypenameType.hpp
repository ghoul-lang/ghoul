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

#ifndef GULC_TEMPLATETYPENAMETYPE_HPP
#define GULC_TEMPLATETYPENAMETYPE_HPP

#include <AST/Type.hpp>
#include <vector>

namespace gulc {
    class TemplateTypenameType : public Type {
    public:
        static bool classof(const Type *expr) { return expr->getTypeKind() == Kind::TemplateTypename; }

        TemplateTypenameType(TextPosition startPosition, TextPosition endPosition)
                : Type(Kind::TemplateTypename, startPosition, endPosition) {}

        std::string getString() const override { return "typename"; }

        Type* deepCopy() const override {
            return new TemplateTypenameType(startPosition(), endPosition());
        }

    };
}

#endif //GULC_TEMPLATETYPENAMETYPE_HPP
