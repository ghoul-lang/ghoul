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

#include "Inheriter.hpp"
#include <AST/Decls/NamespaceDecl.hpp>
#include <AST/Decls/StructDecl.hpp>
#include <AST/Decls/FunctionDecl.hpp>
#include <ASTHelpers/FunctionComparer.hpp>

using namespace gulc;

void Inheriter::processFile(std::vector<FileAST *> &files) {
    for (FileAST* fileAst : files) {
        for (Decl *decl : fileAst->topLevelDecls()) {
            processDecl(decl);
        }
    }
}

void Inheriter::processDecl(Decl *decl) {
    if (llvm::isa<NamespaceDecl>(decl)) {
        processNamespaceDecl(llvm::dyn_cast<NamespaceDecl>(decl));
    } else if (llvm::isa<StructDecl>(decl)) {
        processStructDecl(llvm::dyn_cast<StructDecl>(decl));
    }
}

void Inheriter::processNamespaceDecl(NamespaceDecl *namespaceDecl) {
    for (Decl* nestedDecl : namespaceDecl->nestedDecls()) {
        processDecl(nestedDecl);
    }
}

void Inheriter::processStructDecl(StructDecl *structDecl) {
    if (structDecl->baseStruct != nullptr) {
        // Loop through all inherited structs, check if they should be in our inherited member list, and add them to
        // the list if they should be.
        for (StructDecl* inheritedBase = structDecl->baseStruct;
             inheritedBase != nullptr;
             inheritedBase = inheritedBase->baseStruct) {
            // Loop through the members and add them to the list when necessary.
            for (Decl* inheritMember : inheritedBase->members) {
                // Check if we should inherit the member
                if (structShouldInheritMember(structDecl, inheritMember)) {
                    structDecl->inheritedMembers.push_back(inheritMember);
                }
            }
        }
    }
}

bool Inheriter::structShouldInheritMember(StructDecl *checkStruct, Decl *checkMember) {
    // If the member is private we CANNOT inherit it
    if (checkMember->visibility() == Decl::Visibility::Private) {
        return false;
    }

    for (Decl* member : checkStruct->members) {
        if (member->name() == checkMember->name()) {
            // If the member is a variable then we shadow all other members with the same name, cannot inherit `checkMember`
            if (llvm::isa<GlobalVariableDecl>(member)) {
                return false;
            } else if (llvm::isa<FunctionDecl>(member)) {
                if (llvm::isa<FunctionDecl>(checkMember)) {
                    auto structFunction = llvm::dyn_cast<FunctionDecl>(member);
                    auto checkFunction = llvm::dyn_cast<FunctionDecl>(checkMember);

                    // TODO: Once we support const functions we will have to not compare non-const with const
                    if (FunctionComparer::compare(structFunction, checkFunction) ==
                        FunctionComparer::CompareResult::Identical) {
                        // If they are identical then we shadow `checkMember`
                        return false;
                    }
                } else if (llvm::isa<TemplateFunctionDecl>(checkMember)) {
                    // We don't have to check for similarity in template functions and functions due to the face that
                    // functions and template functions can be differentiated between
                    // We keep searching for potential collisions
                    continue;
                } else {
                    // If the check member is a variable but the member from the struct is a function then the function
                    // shadows the check member
                    return false;
                }
            } else if (llvm::isa<TemplateFunctionDecl>(member)) {
                if (llvm::isa<TemplateFunctionDecl>(checkMember)) {
                    auto structTemplateFunction = llvm::dyn_cast<TemplateFunctionDecl>(member);
                    auto checkTemplateFunction = llvm::dyn_cast<TemplateFunctionDecl>(checkMember);

                    // TODO: Once we support const functions we will have to not compare non-const with const
                    if (FunctionComparer::compare(structTemplateFunction, checkTemplateFunction) ==
                        FunctionComparer::CompareResult::Identical) {
                        // If they are identical then we shadow `checkMember`
                        return false;
                    }
                } else if (llvm::isa<FunctionDecl>(checkMember)) {
                    // We don't have to check for similarity in template functions and functions due to the face that
                    // functions and template functions can be differentiated between
                    // We keep searching for potential collisions
                    continue;
                } else {
                    // If the check member is a variable but the member from the struct is a function then the function
                    // shadows the check member
                    return false;
                }
            }
        }
    }

    return true;
}
