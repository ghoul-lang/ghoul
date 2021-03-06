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
#include <AST/Types/FunctionPointerType.hpp>
#include <AST/Types/PointerType.hpp>
#include <AST/Types/ReferenceType.hpp>
#include <AST/Types/StructType.hpp>
#include <AST/Types/VTableType.hpp>
#include <AST/Types/FlatArrayType.hpp>
#include <AST/Exprs/IntegerLiteralExpr.hpp>
#include <iostream>
#include "SizeofHelper.hpp"

gulc::SizeAndAlignment gulc::SizeofHelper::getSizeAndAlignmentOf(Target* target, gulc::Type *type) {
    if (llvm::isa<BuiltInType>(type)) {
        auto builtInType = llvm::dyn_cast<BuiltInType>(type);

        return gulc::SizeAndAlignment(builtInType->size(), builtInType->size());
    } else if (llvm::isa<EnumType>(type)) {
        auto enumType = llvm::dyn_cast<EnumType>(type);

        return getSizeAndAlignmentOf(target, enumType->baseType());
    } else if (llvm::isa<FunctionPointerType>(type) || llvm::isa<PointerType>(type) ||
               llvm::isa<ReferenceType>(type)) {
        return gulc::SizeAndAlignment(target->sizeofPtr(), target->sizeofPtr());
    } else if (llvm::isa<StructType>(type)) {
        auto structType = llvm::dyn_cast<StructType>(type);
        auto structSize = structType->decl()->completeSizeWithoutPad;

        // We have to correctly add the padding...
        structSize += target->alignofStruct() - (structSize % target->alignofStruct());

        return gulc::SizeAndAlignment(structSize, target->alignofStruct());
    } else if (llvm::isa<VTableType>(type)) {
        return gulc::SizeAndAlignment(target->sizeofPtr(), target->sizeofPtr());
    } else if (llvm::isa<FlatArrayType>(type)) {
        auto flatArrayType = llvm::dyn_cast<FlatArrayType>(type);

        gulc::SizeAndAlignment indexSizeAndAlign = getSizeAndAlignmentOf(target, flatArrayType->indexType);

        std::size_t flatArraySize = 0;

        if (!llvm::isa<IntegerLiteralExpr>(flatArrayType->length)) {
            std::cerr << "[INTERNAL ERROR] flat array type found in `SizeOfHelper` with length type that is NOT an integer!" << std::endl;
            std::exit(1);
        }

        auto integerLiteral = llvm::dyn_cast<IntegerLiteralExpr>(flatArrayType->length);

        if (target->sizeofUSize() == 8) {
            flatArraySize = std::stoull(integerLiteral->numberString, nullptr, integerLiteral->numberBase());
        } else {
            // Assume 32 bit
            flatArraySize = std::stoul(integerLiteral->numberString, nullptr, integerLiteral->numberBase());
        }

        flatArraySize *= indexSizeAndAlign.size;

        return gulc::SizeAndAlignment(flatArraySize, indexSizeAndAlign.align);
    }

    // Anything else is zero since we don't handle errors here
    return gulc::SizeAndAlignment(0, 0);
}
