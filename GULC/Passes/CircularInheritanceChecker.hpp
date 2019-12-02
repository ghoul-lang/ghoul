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

#ifndef GULC_CIRCULARINHERITANCECHECKER_HPP
#define GULC_CIRCULARINHERITANCECHECKER_HPP

#include <vector>
#include <AST/FileAST.hpp>

namespace gulc {
    // Before we can handle actually inheriting from other classes we have to validate that the inheritance lists
    // do not have circular references
    class CircularInheritanceChecker {
    private:
        FileAST* currentFileAst;

    public:
        CircularInheritanceChecker() : currentFileAst(nullptr) {}

        void processFile(std::vector<FileAST*>& files);

    private:
        void printError(const std::string& message, TextPosition startPosition, TextPosition endPosition);

        void processDecl(Decl* decl);

        void processNamespaceDecl(NamespaceDecl* namespaceDecl);
        void processStructDecl(StructDecl* structDecl);

    };
}

#endif //GULC_CIRCULARINHERITANCECHECKER_HPP
