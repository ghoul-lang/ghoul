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

#ifndef GULC_ATTRIBUTEDECL_HPP
#define GULC_ATTRIBUTEDECL_HPP

#include <AST/Decl.hpp>

namespace gulc {
    class AttributeDecl : public Decl {
    public:
        static bool classof(const Decl *decl) { return decl->getDeclKind() == Kind::Attribute; }

        AttributeDecl(std::vector<Attr*> attributes, std::string name, std::string sourceFile,
                      TextPosition startPosition, TextPosition endPosition)
                : Decl(Kind::Attribute, std::move(attributes), std::move(name), std::move(sourceFile),
                       startPosition, endPosition, Visibility::Unspecified) {}

        // TODO

    };
}

#endif //GULC_ATTRIBUTEDECL_HPP
