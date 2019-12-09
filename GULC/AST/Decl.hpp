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
#include <vector>
#include "llvm/Support/Casting.h"
#include "Attr.hpp"

namespace gulc {
    class NamespaceDecl;
    class StructDecl;

    class Decl {
    public:
        enum class Visibility {
            Unspecified,
            Public,
            Private,
            Internal,
            Protected,
            ProtectedInternal
        };

        enum class Kind {
            TemplateFunction,
            Function,
            // TODO: Create 'Property'
            Property,
            MemberFunction,
            MemberProperty,
            Parameter,
            TemplateParameter,

            GlobalVariable,

            Enum,
            EnumConstant,

            Namespace,

            Struct,
            Class,
            Interface,

            Constructor,
            Destructor,

            Attribute
        };

        Kind getDeclKind() const { return _kind; }
        std::vector<Attr*>& attributes() { return _attributes; }
        std::vector<Attr*> const& attributes() const { return _attributes; }
        std::string name() const { return _name; }
        std::string sourceFile() const { return _sourceFile; }
        TextPosition startPosition() const { return _startPosition; }
        TextPosition endPosition() const { return _endPosition; }

        std::string mangledName() const { return _mangledName; }
        void setMangledName(std::string mangledName) { _mangledName = std::move(mangledName); }

        virtual Decl* deepCopy() const = 0;

        virtual ~Decl() = default;

        NamespaceDecl* parentNamespace;
        StructDecl* parentStruct;

        Visibility visibility() const { return _visibility; }
        void setVisibility(Visibility visibility) { _visibility = visibility; }

        bool hasAttributes() const { return !_attributes.empty(); }

        void addAttributes(std::vector<Attr*> const& newAttributes) {
            for (Attr* newAttribute : newAttributes) {
                _attributes.push_back(newAttribute->deepCopy());
            }
        }

        void setAttributes(std::vector<Attr*>& newAttributes) {
            _attributes = newAttributes;
        }

        template<typename T>
        bool hasAttribute() {
            for (Attr* attr : _attributes) {
                if (llvm::isa<T>(attr)) {
                    return true;
                }
            }

            return false;
        }

    protected:
        Decl(Kind kind, std::vector<Attr*> attributes, std::string name, std::string sourceFile,
             TextPosition startPosition, TextPosition endPosition, Visibility visibility)
                : parentNamespace(nullptr), parentStruct(nullptr), _kind(kind), _attributes(std::move(attributes)),
                  _name(std::move(name)), _sourceFile(std::move(sourceFile)), _startPosition(startPosition),
                  _endPosition(endPosition), _mangledName(), _visibility(visibility) {}

    private:
        const Kind _kind;

    protected:
        std::vector<Attr*> _attributes;

    private:
        const std::string _name;
        const std::string _sourceFile;
        const TextPosition _startPosition;
        const TextPosition _endPosition;
        std::string _mangledName;
        Visibility _visibility;

    };
}

#endif //GULC_DECL_HPP
