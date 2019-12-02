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

#include <set>
#include <AST/Decls/StructDecl.hpp>
#include "CircularInheritanceChecker.hpp"

using namespace gulc;

void CircularInheritanceChecker::processFile(std::vector<FileAST *> &files) {
    for (FileAST* fileAst : files) {
        currentFileAst = fileAst;

        for (Decl *decl : fileAst->topLevelDecls()) {
            processDecl(decl);
        }
    }
}

void CircularInheritanceChecker::printError(const std::string &message, TextPosition startPosition,
                                            TextPosition endPosition) {
    std::cout << "gulc resolver error[" << currentFileAst->filePath() << ", "
                                     "{" << startPosition.line << ", " << startPosition.column << "} "
                                     "to {" << endPosition.line << ", " << endPosition.column << "}]: "
              << message
              << std::endl;
    std::exit(1);
}

void CircularInheritanceChecker::processDecl(Decl *decl) {
    if (llvm::isa<NamespaceDecl>(decl)) {
        processNamespaceDecl(llvm::dyn_cast<NamespaceDecl>(decl));
    } else if (llvm::isa<StructDecl>(decl)) {
        processStructDecl(llvm::dyn_cast<StructDecl>(decl));
    }
}

void CircularInheritanceChecker::processNamespaceDecl(NamespaceDecl *namespaceDecl) {
    for (Decl* nestedDecl : namespaceDecl->nestedDecls()) {
        processDecl(nestedDecl);
    }
}

void CircularInheritanceChecker::processStructDecl(StructDecl *structDecl) {
    // We only have to check the `baseStruct` since `trait`s cannot implement new members and cannot extend structs
    std::set<StructDecl*> inheritedStructs;

    if (structDecl->baseStruct == structDecl) {
        printError("structs cannot extend from themselves!",
                   structDecl->startPosition(), structDecl->endPosition());
    }

    StructDecl* lastBaseStruct = nullptr;

    // Loop through all base structs and check for any circular references
    for (StructDecl* baseStruct = structDecl->baseStruct; baseStruct != nullptr; baseStruct = baseStruct->baseStruct) {
        if (baseStruct == structDecl) {
            if (lastBaseStruct == nullptr) {
                lastBaseStruct = baseStruct;
            }

            // This branch is only here to try to improve on error message descriptiveness
            // If `baseStruct == structDecl` we want to print the name of the struct that came right before this struct
            // that is why we print `lastBaseStruct`
            printError("struct `" + structDecl->name() +
                       "` has an illegal circular reference caused by the struct `" + lastBaseStruct->name() + "`!",
                       structDecl->startPosition(), structDecl->endPosition());
        } else if (inheritedStructs.count(baseStruct) > 0) {
            printError("struct `" + structDecl->name() +
                       "` has an illegal circular reference caused by the struct `" + baseStruct->name() + "`!",
                       structDecl->startPosition(), structDecl->endPosition());
        } else {
            inheritedStructs.insert(baseStruct);
        }

        lastBaseStruct = baseStruct;
    }
}
