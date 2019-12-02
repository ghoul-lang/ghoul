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

#ifndef GULC_INHERITER_HPP
#define GULC_INHERITER_HPP

#include <AST/Decl.hpp>
#include <vector>
#include <AST/FileAST.hpp>
#include <Targets/Target.hpp>

namespace gulc {
    /**
     * The Inheriter pass handles giving structs the members that they inherit. This pass has to be performed after
     * `TypeResolver` but before `DeclResolver` due to the fact that `TypeResolver` resolves the referenced base types
     * and `DeclResolver` has to be able to access the inherited members
     *
     * NOTE: This is not meant to be spelled `Inheritor` as `Inheritor` implies that this class inherits something.
     *       I'm spelling it `Inheriter` to imply that it is performing the inheriting on the `StructDecls`
     */
    // TODO: Maybe this should be renamed to `InheritanceResolver` or something similar?
    class Inheriter {
    private:
        Target* _target;
        FileAST* currentFileAst;

    public:
        explicit Inheriter(Target* target) : _target(target), currentFileAst(nullptr) {}

        void processFile(std::vector<FileAST*>& files);

    private:
        void printError(const std::string& message, TextPosition startPosition, TextPosition endPosition);

        void processDecl(Decl* decl);

        void processNamespaceDecl(NamespaceDecl* namespaceDecl);
        void processStructDecl(StructDecl* structDecl);

        // `printShadowWarnings` tells the function to print warnings when a member of the struct shadows the
        // `checkMember`
        // TODO: This might be better to split this into its own class?
        bool structShouldInheritMember(StructDecl* checkStruct, Decl* checkMember);

        // This is used to verify none of the data members uses the `structType` as a value type
        // (i.e. a pointer or reference to `structType` is ok)
        bool structUsesStructTypeAsValue(StructDecl* structType, StructDecl* checkStruct, bool checkBaseStruct);

    };
}

#endif //GULC_INHERITER_HPP
