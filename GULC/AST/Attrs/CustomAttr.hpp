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

#ifndef GULC_CUSTOMATTR_HPP
#define GULC_CUSTOMATTR_HPP

#include <AST/Attr.hpp>
#include <AST/Expr.hpp>
#include <AST/Decls/AttributeDecl.hpp>
#include <vector>

namespace gulc {
    class CustomAttr : public Attr {
    public:
        static bool classof(const Attr *attr) { return attr->getAttrKind() == Kind::Custom; }

        CustomAttr(std::string const& name, TextPosition const& startPosition, TextPosition const& endPosition,
                   AttributeDecl* refAttributeDecl, std::vector<Expr*> arguments)
                : Attr(Kind::Custom, name, startPosition, endPosition),
                  refAttributeDecl(refAttributeDecl), arguments(std::move(arguments)) {}

        // We don't own this so we don't free it
        AttributeDecl* refAttributeDecl;
        std::vector<Expr*> arguments;

        Attr* deepCopy() const override {
            std::vector<Expr*> copiedArguments;

            for (Expr* argument : arguments) {
                copiedArguments.push_back(argument->deepCopy());
            }

            return new CustomAttr(name(), startPosition(), endPosition(),
                                  refAttributeDecl, copiedArguments);
        }

        ~CustomAttr() override {
            for (Expr* argument : arguments) {
                delete argument;
            }
        }

    };
}

#endif //GULC_CUSTOMATTR_HPP
