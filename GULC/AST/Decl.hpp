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

#ifndef GULC_DECL_HPP
#define GULC_DECL_HPP

#include <string>
#include <MetaData/TextPosition.hpp>
#include "llvm/Support/Casting.h"

namespace gulc {
    class Decl {
    public:
        enum class Kind {
            Function,
            // TODO: Add 'MemberFunctionDecl' (a.k.a. MethodDecl) and 'MemberPropertyDecl' (which is why we call it 'MemberFunctionDecl' instead of 'MethodDecl')
            // TODO: Create 'Property'
            Property,
            MemberFunction,
            MemberProperty,
            Parameter,
            TemplateParameter,

            GlobalVariable,

            Enum,
            EnumConstant,

            Namespace
        };

        Kind getDeclKind() const { return _kind; }
        std::string name() const { return _name; }
        std::string sourceFile() const { return _sourceFile; }
        TextPosition startPosition() const { return _startPosition; }
        TextPosition endPosition() const { return _endPosition; }

        std::string mangledName() const { return _mangledName; }
        void setMangledName(std::string mangledName) { _mangledName = std::move(mangledName); }

        virtual ~Decl() = default;

    protected:
        Decl(Kind kind, std::string name, std::string sourceFile, TextPosition startPosition, TextPosition endPosition)
                : _kind(kind), _name(std::move(name)),
                  _sourceFile(std::move(sourceFile)), _startPosition(startPosition), _endPosition(endPosition) {}

    private:
        const Kind _kind;
        const std::string _name;
        const std::string _sourceFile;
        const TextPosition _startPosition;
        const TextPosition _endPosition;
        std::string _mangledName;

    };
}

#endif //GULC_DECL_HPP
