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

#include <llvm/Support/Casting.h>
#include <AST/Types/ConstType.hpp>
#include <AST/Types/MutType.hpp>
#include <AST/Types/ImmutType.hpp>
#include <AST/Types/BuiltInType.hpp>
#include <AST/Types/EnumType.hpp>
#include <AST/Types/PointerType.hpp>
#include <AST/Types/ReferenceType.hpp>
#include <AST/Types/FunctionPointerType.hpp>
#include <iostream>
#include <AST/Types/StructType.hpp>
#include "TypeComparer.hpp"

using namespace gulc;

// TODO: Should we just combine with `getTypeGreaterThan` as a `compareTypes`?
// TODO: `ignoreQualifiers` should only ignore the first qualifiers...
bool TypeComparer::getTypesAreSame(const Type* type1, const Type* type2, bool ignoreQualifiers) {
    if (type1->getTypeKind() == type2->getTypeKind()) {
        switch (type1->getTypeKind()) {
            case Type::Kind::Const: {
                auto constType1 = llvm::dyn_cast<ConstType>(type1);
                auto constType2 = llvm::dyn_cast<ConstType>(type2);

                return getTypesAreSame(constType1->pointToType, constType2->pointToType, ignoreQualifiers);
            }
            case Type::Kind::Mut: {
                auto mutType1 = llvm::dyn_cast<MutType>(type1);
                auto mutType2 = llvm::dyn_cast<MutType>(type2);

                return getTypesAreSame(mutType1->pointToType, mutType2->pointToType, ignoreQualifiers);
            }
            case Type::Kind::Immut: {
                auto immutType1 = llvm::dyn_cast<ImmutType>(type1);
                auto immutType2 = llvm::dyn_cast<ImmutType>(type2);

                return getTypesAreSame(immutType1->pointToType, immutType2->pointToType, ignoreQualifiers);
            }
            case Type::Kind::BuiltIn: {
                auto builtInType1 = llvm::dyn_cast<BuiltInType>(type1);
                auto builtInType2 = llvm::dyn_cast<BuiltInType>(type2);

                return builtInType1->name() == builtInType2->name();
            }
            case Type::Kind::Enum: {
                auto enumType1 = llvm::dyn_cast<EnumType>(type1);
                auto enumType2 = llvm::dyn_cast<EnumType>(type2);

                return enumType1->decl() == enumType2->decl();
            }
            case Type::Kind::Struct: {
                auto structType1 = llvm::dyn_cast<StructType>(type1);
                auto structType2 = llvm::dyn_cast<StructType>(type2);

                return structType1->decl() == structType2->decl();
            }
            case Type::Kind::FunctionTemplateTypenameRef: {
                auto pointerType1 = llvm::dyn_cast<PointerType>(type1);
                auto pointerType2 = llvm::dyn_cast<PointerType>(type2);

                return getTypesAreSame(pointerType1->pointToType, pointerType2->pointToType, ignoreQualifiers);
            }
            case Type::Kind::Pointer: {
                auto pointerType1 = llvm::dyn_cast<PointerType>(type1);
                auto pointerType2 = llvm::dyn_cast<PointerType>(type2);

                return getTypesAreSame(pointerType1->pointToType, pointerType2->pointToType, ignoreQualifiers);
            }
            case Type::Kind::Reference: {
                auto refType1 = llvm::dyn_cast<ReferenceType>(type1);
                auto refType2 = llvm::dyn_cast<ReferenceType>(type2);

                return getTypesAreSame(refType1->referenceToType, refType2->referenceToType, ignoreQualifiers);
            }
            case Type::Kind::FunctionPointer: {
                auto funcPointerType1 = llvm::dyn_cast<FunctionPointerType>(type1);
                auto funcPointerType2 = llvm::dyn_cast<FunctionPointerType>(type2);

                if (!getTypesAreSame(funcPointerType1->resultType, funcPointerType2->resultType, ignoreQualifiers)) {
                    return false;
                }

                if (funcPointerType1->paramTypes.empty() != funcPointerType2->paramTypes.empty()) {
                    return false;
                }

                if (!funcPointerType1->paramTypes.empty()) {
                    for (std::size_t i = 0; i < funcPointerType1->paramTypes.size(); ++i) {
                        if (!getTypesAreSame(funcPointerType1->paramTypes[i], funcPointerType2->paramTypes[i], ignoreQualifiers)) {
                            return false;
                        }
                    }
                }

                return true;
            }
            case Type::Kind::TemplateTypename: {
                return true;
            }
            case Type::Kind::Unresolved: {
                std::cout << "gulc qualify type pass [DEBUG WARNING]: attempted to compare two 'UnresolvedType's, operation cannot be completed. Defaulting to false." << std::endl;
                return false;
            }
        }
    } else if (ignoreQualifiers) { // Types are not the same...
        bool typeChanged = false;

        if (llvm::isa<MutType>(type1)) {
            type1 = llvm::dyn_cast<MutType>(type1)->pointToType;
            typeChanged = true;
        } else if (llvm::isa<ConstType>(type1)) {
            type1 = llvm::dyn_cast<ConstType>(type1)->pointToType;
            typeChanged = true;
        } else if (llvm::isa<ImmutType>(type1)) {
            type1 = llvm::dyn_cast<ImmutType>(type1)->pointToType;
            typeChanged = true;
        }

        if (llvm::isa<MutType>(type2)) {
            type2 = llvm::dyn_cast<MutType>(type2)->pointToType;
            typeChanged = true;
        } else if (llvm::isa<ConstType>(type2)) {
            type2 = llvm::dyn_cast<ConstType>(type2)->pointToType;
            typeChanged = true;
        } else if (llvm::isa<ImmutType>(type2)) {
            type2 = llvm::dyn_cast<ImmutType>(type2)->pointToType;
            typeChanged = true;
        }

        if (typeChanged) {
            return getTypesAreSame(type1, type2, ignoreQualifiers);
        }
    }

    return false;
}
