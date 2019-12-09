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

#ifndef GULC_UNRESOLVEDATTR_HPP
#define GULC_UNRESOLVEDATTR_HPP

#include <AST/Attr.hpp>
#include <vector>
#include <AST/Expr.hpp>

namespace gulc {
    class UnresolvedAttr : public Attr {
    public:
        static bool classof(const Attr *attr) { return attr->getAttrKind() == Kind::Unresolved; }

        UnresolvedAttr(std::string const& name, TextPosition const& startPosition, TextPosition const& endPosition,
                       std::vector<std::string> namespacePath, std::vector<Expr*> arguments)
                : Attr(Kind::Unresolved, name, startPosition, endPosition),
                  namespacePath(std::move(namespacePath)),
                  arguments(std::move(arguments)) {}

        std::vector<std::string> namespacePath;
        // NOTE: The attribute name is just `name`
        std::vector<Expr*> arguments;

        Attr* deepCopy() const override {
            std::vector<Expr*> copiedArguments;

            for (Expr* argument : arguments) {
                copiedArguments.push_back(argument->deepCopy());
            }

            return new UnresolvedAttr(name(), startPosition(), endPosition(),
                                      namespacePath, copiedArguments);
        }

        ~UnresolvedAttr() override {
            for (Expr* argument : arguments) {
                delete argument;
            }
        }

    };
}

#endif //GULC_UNRESOLVEDATTR_HPP
