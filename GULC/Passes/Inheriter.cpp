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
#include <AST/Types/EnumType.hpp>
#include <AST/Types/FunctionPointerType.hpp>
#include <AST/Types/PointerType.hpp>
#include <AST/Types/ReferenceType.hpp>
#include <AST/Types/StructType.hpp>
#include <ASTHelpers/SizeofHelper.hpp>
#include <make_reverse_iterator.hpp>
#include <AST/Types/FlatArrayType.hpp>

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
    // This function might be called more than once for the same struct decl. Because of this we do a check to see if
    // the struct already has inherited members. If it does then we assume we've already processed the struct.
    if (structDecl->inheritedMembers.empty()) {
        std::vector<std::vector<GlobalVariableDecl*>*> allInheritedDataMembers;

        //allDataMembers.push_back(&structDecl->dataMembers);

        if (structDecl->baseStruct != nullptr) {
            // Loop through all inherited structs, check if they should be in our inherited member list, and add them to
            // the list if they should be.
            for (StructDecl *inheritedBase = structDecl->baseStruct;
                 inheritedBase != nullptr;
                 inheritedBase = inheritedBase->baseStruct) {
                // Loop through the members and add them to the list when necessary.
                for (Decl *inheritMember : inheritedBase->members) {
                    // Check if we should inherit the member
                    if (structShouldInheritMember(structDecl, inheritMember)) {
                        structDecl->inheritedMembers.push_back(inheritMember);
                    }
                }

                // Add the inherited data members to the `allDataMembers` list
                allInheritedDataMembers.push_back(&inheritedBase->dataMembers);
            }
        }

        std::size_t currentSize = 0;

        // Loop through all pointers to struct declaration data members from the greatest grandparent to the direct parent
        for (std::vector<GlobalVariableDecl*>* dataMembers : gulc::reverse(allInheritedDataMembers)) {
            // Loop through each data member normally
            for (GlobalVariableDecl *dataMember : *dataMembers) {
                SizeAndAlignment sizeAndAlignment(0, 0);

                if (llvm::isa<StructType>(dataMember->type)) {
                    auto structType = llvm::dyn_cast<StructType>(dataMember->type);

                    // If the struct has not already had its inherited members added then we process the struct...
                    if (structType->decl()->inheritedMembers.empty()) {
                        processStructDecl(structType->decl());
                    }
                } else if (llvm::isa<EnumType>(dataMember->type)) {
                    auto enumType = llvm::dyn_cast<EnumType>(dataMember->type);

                    if (llvm::isa<StructType>(enumType->baseType())) {
                        auto structType = llvm::dyn_cast<StructType>(dataMember->type);

                        // If the struct has not already had its inherited members added then we process the struct...
                        if (structType->decl()->inheritedMembers.empty()) {
                            processStructDecl(structType->decl());
                        }
                    }
                }

                sizeAndAlignment = SizeofHelper::getSizeAndAlignmentOf(_target, dataMember->type);

                std::size_t alignPadding = sizeAndAlignment.align - (currentSize % sizeAndAlignment.align);

                // Rather than deal with casting to a signed type and rearrange the above algorithm to prevent
                // this from happening, we just check if the `alignPadding` is equal to the `align` and set
                // `alignPadding` to zero if it happens
                if (alignPadding == sizeAndAlignment.align) {
                    alignPadding = 0;
                }

                // Add padding to give the member the proper alignment
                currentSize += alignPadding;
                // Add the size of the member
                currentSize += sizeAndAlignment.size;
            }
        }

        // I'm separating this out here as I don't know if this will be optimized to only be called once in C++ within
        // a for loop. We need it to be incremented as we modify the list
        std::size_t dataMembersLength = structDecl->dataMembers.size();

        // Loop through the current `structDecl` data members
        for (std::size_t i = 0; i < dataMembersLength; ++i) {
            GlobalVariableDecl* dataMember = structDecl->dataMembers[i];

            SizeAndAlignment sizeAndAlignment(0, 0);

            if (llvm::isa<StructType>(dataMember->type)) {
                auto structType = llvm::dyn_cast<StructType>(dataMember->type);

                // If the struct has not already had its inherited members added then we process the struct...
                if (structType->decl()->inheritedMembers.empty()) {
                    processStructDecl(structType->decl());
                }
            } else if (llvm::isa<EnumType>(dataMember->type)) {
                auto enumType = llvm::dyn_cast<EnumType>(dataMember->type);

                if (llvm::isa<StructType>(enumType->baseType())) {
                    auto structType = llvm::dyn_cast<StructType>(dataMember->type);

                    // If the struct has not already had its inherited members added then we process the struct...
                    if (structType->decl()->inheritedMembers.empty()) {
                        processStructDecl(structType->decl());
                    }
                }
            }

            sizeAndAlignment = SizeofHelper::getSizeAndAlignmentOf(_target, dataMember->type);

            std::size_t alignPadding = 0;

            // `align` can't be zero, `n % 0` is illegal since `n / 0` is illegal
            if (sizeAndAlignment.align != 0) {
                alignPadding = sizeAndAlignment.align - (currentSize % sizeAndAlignment.align);

                // Rather than deal with casting to a signed type and rearrange the above algorithm to prevent
                // this from happening, we just check if the `alignPadding` is equal to the `align` and set
                // `alignPadding` to zero if it happens
                if (alignPadding == sizeAndAlignment.align) {
                    alignPadding = 0;
                }
            }

            // If the `alignPadding` is greater than zero then we add an anonymous variable to the struct for padding
            if (alignPadding > 0) {
                // Create an `i8` type or `byte` type (both are the same)
                auto i8Type = new BuiltInType({}, {}, TypeQualifier::None, "byte");
                // Create the size expression using an integer literal
                auto paddingSize = new IntegerLiteralExpr({}, {}, 10, std::to_string(alignPadding));
                // Create the padding flat array type
                auto paddingType = new FlatArrayType({}, {}, TypeQualifier::None, i8Type, paddingSize);

                // Add padding before the current data member
                structDecl->dataMembers.insert(structDecl->dataMembers.begin() + i,
                                               new GlobalVariableDecl("", structDecl->sourceFile(), {}, {},
                                                                      Decl::Visibility::Private, paddingType));
                // Increment `i` by one so we reset ourselves back to the original data member
                ++i;
                // Increment the data members length so we account for the new data member
                ++dataMembersLength;
            }

            // Add padding to give the member the proper alignment
            currentSize += alignPadding;
            // Add the size of the member
            currentSize += sizeAndAlignment.size;
        }

        structDecl->completeSizeWithoutPad = currentSize;
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
