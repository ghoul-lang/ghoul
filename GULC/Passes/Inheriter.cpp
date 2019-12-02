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
#include <AST/Types/VTableType.hpp>

using namespace gulc;

void Inheriter::processFile(std::vector<FileAST *> &files) {
    for (FileAST* fileAst : files) {
        currentFileAst = fileAst;

        for (Decl *decl : fileAst->topLevelDecls()) {
            processDecl(decl);
        }
    }
}

void Inheriter::printError(const std::string &message, TextPosition startPosition, TextPosition endPosition) {
    std::cout << "gulc resolver error[" << currentFileAst->filePath() << ", "
                                     "{" << startPosition.line << ", " << startPosition.column << "} "
                                     "to {" << endPosition.line << ", " << endPosition.column << "}]: "
              << message
              << std::endl;
    std::exit(1);
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
        // We ALSO have to verify that the current struct and its base structs DO NOT have a value reference of our
        // struct. This is a long process but is required to prevent infinite loops within our compiler
        // We do this check first to prevent any potential issues with inheritance
        // (like circular references in the inheritance list)
        // TODO: We need to improve our checks for potential circular references in the inheritance list...
        for (GlobalVariableDecl* checkVariable : structDecl->dataMembers) {
            if (llvm::dyn_cast<StructType>(checkVariable->type)) {
                auto checkStructType = llvm::dyn_cast<StructType>(checkVariable->type);

                if (checkStructType->decl() == structDecl) {
                    printError("struct members CANNOT be the same type as the structs they are in! (did you mean to make `" + checkVariable->name() + "` a pointer or reference instead?)",
                               checkVariable->startPosition(), checkVariable->endPosition());
                }

                if (structUsesStructTypeAsValue(structDecl, checkStructType->decl(), true)) {
                    // TODO: This error message seems like it could be a little confusing?
                    printError("type of member variable `" + checkVariable->name() + "` uses the current struct `" + structDecl->name() + "` by value in it's members or inheritance, this creates an illegal circular reference!",
                               checkVariable->startPosition(), checkVariable->endPosition());
                }
            }
        }

        if (structDecl->baseStruct != nullptr) {
            if (structUsesStructTypeAsValue(structDecl, structDecl->baseStruct, true)) {
                // TODO: This error message also seems like it could be a little confusing?
                printError("cannot extend from base type `" + structDecl->baseStruct->name() + "`, base type uses current struct `" + structDecl->name() + "` by value which is illegal!",
                           structDecl->startPosition(), structDecl->endPosition());
            }
        }

        // List of all members, current and inherited. Used to properly handle overriding virtual functions
        std::vector<std::vector<Decl*>*> allKnownMembers;
        // List of all inherited data members. Used to properly handle shadowing the data members
        std::vector<std::vector<GlobalVariableDecl*>*> allInheritedDataMembers;

        //allDataMembers.push_back(&structDecl->dataMembers);
        allKnownMembers.push_back(&structDecl->members);

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

                // Add the inherited members to the `allKnownMembers` list
                allKnownMembers.push_back(&inheritedBase->members);
            }
        }

        // Construct this current struct's vtable using the `allKnownMembers`
        // To do this, we loop from the greatest grandparent to the current struct. While doing this we add any virtual
        // functions to the vtable and then replace any functions that are overriding
        for (std::vector<Decl*>* dataMembers : gulc::reverse(allKnownMembers)) {
            for (Decl* checkMember : *dataMembers) {
                if (llvm::isa<FunctionDecl>(checkMember)) {
                    auto checkFunction = llvm::dyn_cast<FunctionDecl>(checkMember);

                    // Check if the function should be in the vtable
                    if (checkFunction->modifier() == FunctionModifiers::Abstract ||
                        checkFunction->modifier() == FunctionModifiers::Virtual ||
                        checkFunction->modifier() == FunctionModifiers::Override) {
                        // If the function should be in the vtable check if a function with the same signature already
                        // exists in the function table
                        bool functionAddedToVtable = false;

                        // We only check if we should replace the vtable entry if it is `override`, you're allowed
                        // to shadow vtable entries.
                        if (checkFunction->modifier() == FunctionModifiers::Override) {
                            // We loop the vtable backwards to account for shadowing.
                            for (std::size_t vtableIndex = structDecl->vtable.size() - 1; vtableIndex >= 0;
                                 --vtableIndex) {
                                FunctionDecl* vtableEntry = structDecl->vtable[vtableIndex];

                                if (vtableEntry->name() == checkFunction->name()) {
                                    // If the names match then we have to check that the signatures match exactly
                                    if (FunctionComparer::compare(vtableEntry, checkFunction) ==
                                        FunctionComparer::CompareResult::Identical) {
                                        // If everything matches then a proper override was found. We replace the entry,
                                        // notify the next if statement we added the function, and break from our search
                                        structDecl->vtable[vtableIndex] = checkFunction;
                                        functionAddedToVtable = true;
                                        break;
                                    }
                                }
                            }
                        }

                        // If it wasn't added to the vtable then we just add it to the end.
                        if (!functionAddedToVtable) {
                            // We do have to verify the `checkFunction` is either abstract or virtual for this to work
                            // properly (if it was override at this point then the function that was being overridden
                            // was not found)
                            // NOTE: We only perform the modifier check if the `checkFunction` is owned by the current
                            //       `structDecl`. If we don't do this check the error will be printed for the wrong
                            //       file.
                            if (checkFunction->parentStruct == structDecl &&
                                checkFunction->modifier() != FunctionModifiers::Abstract &&
                                checkFunction->modifier() != FunctionModifiers::Virtual) {
                                printError("no suitable function found to override! (did you mean `virtual`?)",
                                           checkFunction->startPosition(), checkFunction->endPosition());
                            }

                            structDecl->vtable.push_back(checkFunction);

                            // If the vtable owner isn't set then we set it to the parent struct of the function we're
                            // adding. We can be sure this is the owner because this will be the first virtual function
                            // found in our inheritance
                            if (structDecl->vtableOwner == nullptr) {
                                structDecl->vtableOwner = checkFunction->parentStruct;
                            }
                        }
                    }
                }
            }
        }

        std::size_t currentSize = 0;

        // Loop through all pointers to struct declaration data members from the greatest grandparent to the direct parent
        for (std::vector<GlobalVariableDecl*>* dataMembers : gulc::reverse(allInheritedDataMembers)) {
            // At this point in execution the base struct is not guaranteed to have its vtable member. Because of this,
            // we have to check if the `dataMembers` are owned by the vtable owner and if it is check if the first
            // data member is the vtable. If it isn't we increment `currentSize` by the size of a vtable
            if (!dataMembers->empty()) {
                GlobalVariableDecl* checkVTable = (*dataMembers)[0];

                // If the `checkVTable` is owned by our vtable owner...
                if (checkVTable->parentStruct == structDecl->vtableOwner) {
                    // If the first member isn't a vtable then we have to change `currentSize` as if it was a vtable...
                    if (!llvm::isa<VTableType>(checkVTable->type)) {
                        std::size_t vtableSize = _target->sizeofPtr();
                        std::size_t alignPadding = vtableSize - (currentSize % vtableSize);

                        // Rather than deal with casting to a signed type and rearrange the above algorithm to prevent
                        // this from happening, we just check if the `alignPadding` is equal to the `align` and set
                        // `alignPadding` to zero if it happens
                        if (alignPadding == vtableSize) {
                            alignPadding = 0;
                        }

                        // Add padding to give the member the proper alignment
                        currentSize += alignPadding;
                        // Add the size of the member
                        currentSize += vtableSize;
                    }
                }
            }

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

        // Since the current `structDecl` might be the vtable owner we have to check for that...
        if (structDecl->vtableOwner == structDecl) {
            // We just add it to the beginning of our data members. We'll let the loop below handle the size normally
            auto vtableMember = new GlobalVariableDecl("", "", {}, {}, Decl::Visibility::Private,
                                                       new VTableType({}, {}));
            structDecl->dataMembers.insert(structDecl->dataMembers.begin(), vtableMember);
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


bool Inheriter::structUsesStructTypeAsValue(StructDecl *structType, StructDecl *checkStruct, bool checkBaseStruct) {
    if (checkStruct == structType) {
        // If the check struct IS struct type we return true
        return true;
    }

    for (GlobalVariableDecl* checkVariable : checkStruct->dataMembers) {
        if (llvm::isa<StructType>(checkVariable->type)) {
            auto checkStructType = llvm::dyn_cast<StructType>(checkVariable->type);

            if (checkStructType->decl() == structType) {
                // If the member's type is `structType` we immediately stop searching and return true
                return true;
            } else {
                // If it isn't the `structType` we also have to check the type to see if it uses `structType`
                if (structUsesStructTypeAsValue(structType, checkStructType->decl(), true)) {
                    return true;
                }
            }
        }
    }

    if (checkBaseStruct) {
        // If the base struct IS the struct type we return true
        if (checkStruct->baseStruct == structType) {
            return true;
        }

        // We also have to check the base classes for it...
        for (StructDecl *checkBase = checkStruct->baseStruct; checkBase != nullptr;
             checkBase = checkBase->baseStruct) {
            // If the base type uses `structType` we immediately return true...
            if (structUsesStructTypeAsValue(structType, checkBase, true)) {
                return true;
            }
        }
    }

    // If we reach this point then `checkStruct` doesn't implement `structType` in any way as a value type
    return false;
}
