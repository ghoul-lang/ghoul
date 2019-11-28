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

#ifndef GULC_TYPE_HPP
#define GULC_TYPE_HPP

#include <MetaData/TextPosition.hpp>
#include <string>

namespace gulc {
    enum class TypeQualifier {
        None,
        Mut,
        Const
    };

    class Type {
    public:
        enum class Kind {
            TemplateTypename,
            Unresolved,
            FunctionTemplateTypenameRef,
            BuiltIn,
            Pointer,
            FunctionPointer,
            Reference,
            Enum,
            Struct,
            FlatArray
        };

        TypeQualifier qualifier() const { return _qualifier; }
        void setQualifier(TypeQualifier qualifier) { _qualifier = qualifier; }
        Kind getTypeKind() const { return _kind; }
        TextPosition startPosition() const { return _startPosition; }
        TextPosition endPosition() const { return _endPosition; }
        virtual std::string getString() const = 0;

        virtual bool isLValue() const { return _isLValue; }
        virtual void setIsLValue(bool isLValue) { _isLValue = isLValue; }

        virtual Type* deepCopy() const = 0;

        virtual ~Type() = default;

    protected:
        Type(Kind kind, TextPosition startPosition, TextPosition endPosition, TypeQualifier qualifier)
                : _qualifier(qualifier), _kind(kind), _startPosition(startPosition), _endPosition(endPosition),
                  _isLValue(false) {}

    private:
        TypeQualifier _qualifier;
        const Kind _kind;
        const TextPosition _startPosition;
        const TextPosition _endPosition;
        bool _isLValue;

    };
}

#endif //GULC_TYPE_HPP
