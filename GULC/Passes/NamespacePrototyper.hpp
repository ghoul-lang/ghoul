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

#ifndef GULC_NAMESPACEPROTOTYPER_HPP
#define GULC_NAMESPACEPROTOTYPER_HPP

#include <vector>
#include <AST/Decls/NamespaceDecl.hpp>
#include <AST/FileAST.hpp>

namespace gulc {
    /**
     * NamespacePrototyper creates a global list of all namespaces visible to the current project.
     * It creates prototypes of all functions, variables, etc. that can then be use to import lookups and absolute path lookups.
     */
    class NamespacePrototyper {
    public:
        NamespacePrototyper()
                : currentNamespace(nullptr) {}

        std::vector<NamespaceDecl*> generatePrototypes(FileAST& file);

    private:
        NamespaceDecl* getNamespacePrototype(std::vector<NamespaceDecl*>& result, std::string name);

        void generateNamespaceDecl(std::vector<NamespaceDecl*>& result, NamespaceDecl* namespaceDecl);

        NamespaceDecl* currentNamespace;

    };
}

#endif //GULC_NAMESPACEPROTOTYPER_HPP
