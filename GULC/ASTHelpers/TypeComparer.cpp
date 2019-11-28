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
#include <AST/Types/BuiltInType.hpp>
#include <AST/Types/EnumType.hpp>
#include <AST/Types/PointerType.hpp>
#include <AST/Types/ReferenceType.hpp>
#include <AST/Types/FunctionPointerType.hpp>
#include <iostream>
#include <AST/Types/StructType.hpp>
#include <AST/Types/FunctionTemplateTypenameRefType.hpp>
#include "TypeComparer.hpp"

using namespace gulc;

bool TypeComparer::getTypesAreSame(const Type* type1, const Type* type2, bool ignoreFirstQualifier) {
    if (!ignoreFirstQualifier) {
        // This verifies that both are either const or both aren't const
        // When we don't ignore the qualifiers we only care about const. In terms of checking if the types are the same,
        // `None` == `Mut` but `None` != `Const` and `Mut` != `Const`
        if ((type1->qualifier() == TypeQualifier::Const) != (type2->qualifier() == TypeQualifier::Const)) {
            return false;
        }
    }

    if (type1->getTypeKind() == type2->getTypeKind()) {
        switch (type1->getTypeKind()) {
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
                auto functionTemplateTypenameRef1 = llvm::dyn_cast<FunctionTemplateTypenameRefType>(type1);
                auto functionTemplateTypenameRef2 = llvm::dyn_cast<FunctionTemplateTypenameRefType>(type2);

                return functionTemplateTypenameRef1->templateParameterIndex() ==
                       functionTemplateTypenameRef2->templateParameterIndex();
            }
            case Type::Kind::Pointer: {
                auto pointerType1 = llvm::dyn_cast<PointerType>(type1);
                auto pointerType2 = llvm::dyn_cast<PointerType>(type2);

                return getTypesAreSame(pointerType1->pointToType, pointerType2->pointToType, false);
            }
            case Type::Kind::Reference: {
                auto refType1 = llvm::dyn_cast<ReferenceType>(type1);
                auto refType2 = llvm::dyn_cast<ReferenceType>(type2);

                return getTypesAreSame(refType1->referenceToType, refType2->referenceToType, false);
            }
            case Type::Kind::FunctionPointer: {
                auto funcPointerType1 = llvm::dyn_cast<FunctionPointerType>(type1);
                auto funcPointerType2 = llvm::dyn_cast<FunctionPointerType>(type2);

                if (!getTypesAreSame(funcPointerType1->resultType, funcPointerType2->resultType, false)) {
                    return false;
                }

                if (funcPointerType1->paramTypes.empty() != funcPointerType2->paramTypes.empty()) {
                    return false;
                }

                if (!funcPointerType1->paramTypes.empty()) {
                    for (std::size_t i = 0; i < funcPointerType1->paramTypes.size(); ++i) {
                        if (!getTypesAreSame(funcPointerType1->paramTypes[i], funcPointerType2->paramTypes[i], false)) {
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
    }

    return false;
}
