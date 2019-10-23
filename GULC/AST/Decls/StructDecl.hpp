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

#ifndef GULC_STRUCTDECL_HPP
#define GULC_STRUCTDECL_HPP

#include <AST/Decl.hpp>
#include <vector>
#include "GlobalVariableDecl.hpp"

namespace gulc {
    class StructDecl : public Decl {
    public:
        static bool classof(const Decl *decl) { return decl->getDeclKind() == Kind::Struct; }

        StructDecl(std::string name, std::string sourceFile, TextPosition startPosition, TextPosition endPosition,
                   std::vector<Decl*> members)
                : Decl(Kind::Struct, std::move(name), std::move(sourceFile), startPosition, endPosition),
                  members(std::move(members)) {}

        // These are just sorted references to already existing members within the `members` list, we do NOT free these as that would cause a double free issue.
        std::vector<GlobalVariableDecl*> dataMembers;

        std::vector<Decl*> members;

        Decl* deepCopy() const override {
            std::vector<Decl*> copiedMembers{};

            for (Decl* decl : members) {
                copiedMembers.push_back(decl->deepCopy());
            }

            return new StructDecl(name(), sourceFile(),
                                  startPosition(), endPosition(),
                                  std::move(copiedMembers));
        }

        ~StructDecl() override {
            for (Decl* decl : members) {
                delete decl;
            }
        }

    };
}

#endif //GULC_STRUCTDECL_HPP
