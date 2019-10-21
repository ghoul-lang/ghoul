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

#include "NamespacePrototyper.hpp"

using namespace gulc;

std::vector<NamespaceDecl*> gulc::NamespacePrototyper::generatePrototypes(std::vector<FileAST*>& files) {
    std::vector<NamespaceDecl *> result{};

    for (FileAST* fileAst : files) {
        for (Decl *decl : fileAst->topLevelDecls()) {
            if (llvm::isa<NamespaceDecl>(decl)) {
                generateNamespaceDecl(result, llvm::dyn_cast<NamespaceDecl>(decl));
            }
        }
    }

    return std::move(result);
}

NamespaceDecl *NamespacePrototyper::getNamespacePrototype(std::vector<NamespaceDecl *> &result, std::string name) {
    if (currentNamespace == nullptr) {
        for (NamespaceDecl *namespaceDecl : result) {
            if (namespaceDecl->name() == name) {
                return namespaceDecl;
            }
        }
    } else {
        for (Decl* checkDecl : currentNamespace->nestedDecls()) {
            if (llvm::isa<NamespaceDecl>(checkDecl)) {
                if (checkDecl->name() == name) {
                    return llvm::dyn_cast<NamespaceDecl>(checkDecl);
                }
            }
        }
    }

    auto newNamespace = new NamespaceDecl(name, "[prototype]", {}, {});
    newNamespace->makePrototype();

    if (currentNamespace == nullptr) {
        result.push_back(newNamespace);
    } else {
        currentNamespace->addNestedDecl(newNamespace);
    }

    return newNamespace;
}

void NamespacePrototyper::generateNamespaceDecl(std::vector<NamespaceDecl*>& result, NamespaceDecl *namespaceDecl) {
    NamespaceDecl* oldNamespace = currentNamespace;
    currentNamespace = getNamespacePrototype(result, namespaceDecl->name());

    for (Decl* decl : namespaceDecl->nestedDecls()) {
        if (llvm::isa<NamespaceDecl>(decl)) {
            generateNamespaceDecl(result, llvm::dyn_cast<NamespaceDecl>(decl));
        } else {
            currentNamespace->addNestedDecl(decl);
        }
    }

    currentNamespace = oldNamespace;
}
