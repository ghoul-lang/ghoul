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

#ifndef GULC_FUNCTIONTEMPLATETYPENAMEREFTYPE_HPP
#define GULC_FUNCTIONTEMPLATETYPENAMEREFTYPE_HPP

#include <AST/Type.hpp>
#include <string>

namespace gulc {
    class FunctionTemplateTypenameRefType : public Type {
    public:
        static bool classof(const Type *expr) { return expr->getTypeKind() == Kind::FunctionTemplateTypenameRef; }

        FunctionTemplateTypenameRefType(TextPosition startPosition, TextPosition endPosition,
                                        std::size_t templateParameterIndex)
                : Type(Kind::FunctionTemplateTypenameRef, startPosition, endPosition),
                  _templateParameterIndex(templateParameterIndex) {}

        std::size_t templateParameterIndex() const { return _templateParameterIndex; }
        std::string getString() const override { return std::to_string(_templateParameterIndex); }

        Type* deepCopy() const override {
            return new FunctionTemplateTypenameRefType(startPosition(), endPosition(),
                                                       _templateParameterIndex);
        }

    private:
        std::size_t _templateParameterIndex;

    };
}

#endif //GULC_FUNCTIONTEMPLATETYPENAMEREFTYPE_HPP
