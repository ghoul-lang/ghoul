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

#include <AST/Types/PointerType.hpp>
#include <AST/Types/ReferenceType.hpp>
#include <AST/Types/RValueReferenceType.hpp>
#include <AST/Types/ConstType.hpp>
#include <AST/Types/MutType.hpp>
#include <AST/Types/ImmutType.hpp>
#include <AST/Types/BuiltInType.hpp>
#include <iostream>
#include <AST/Types/EnumType.hpp>
#include "ItaniumMangler.hpp"

using namespace gulc;
//https://itanium-cxx-abi.github.io/cxx-abi/abi.html

std::string ItaniumMangler::mangle(FunctionDecl *functionDecl) {
    // All mangled names start with "_Z"...
    std::string mangledName = "_Z";

    // We currently don't have namespaces so we only support <unscoped-name>
    mangledName += sourceName(functionDecl->name());

    // We only have to use <bare-function-name> since there isn't a namespace yet.
    mangledName += bareFunctionType(functionDecl->parameters);

    return mangledName;
}

std::string ItaniumMangler::mangle(GlobalVariableDecl *globalVariableDecl) {
    // All mangled names start with "_Z"...
    std::string mangledName = "_Z";

    // We currently don't have namespaces so we only support <unscoped-name>
    mangledName += sourceName(globalVariableDecl->name());

    return mangledName;
}

std::string ItaniumMangler::bareFunctionType(std::vector<ParameterDecl*> &params) {
    std::string result;

    for (ParameterDecl* param : params) {
        Type* genType = param->type;

        // TODO: Should we ignore `mut` and `immut` here?
        if (llvm::isa<ConstType>(genType)) {
            genType = llvm::dyn_cast<ConstType>(genType)->pointToType;
        } else if (llvm::isa<ImmutType>(genType)) {
            genType = llvm::dyn_cast<ImmutType>(genType)->pointToType;
        } else if (llvm::isa<MutType>(genType)) {
            genType = llvm::dyn_cast<MutType>(genType)->pointToType;
        }

        result += typeName(genType);
    }

    return result;
}

std::string ItaniumMangler::typeName(gulc::Type *type) {
    if (llvm::isa<BuiltInType>(type)) {
        auto builtInType = llvm::dyn_cast<BuiltInType>(type);
        const std::string checkName = builtInType->name();

        if (checkName == "void") {
            return "v";
        } else if (checkName == "bool") {
            return "b";
        } else {
            return sourceName(checkName);
        }
    } else if (llvm::isa<EnumType>(type)) {
        auto enumType = llvm::dyn_cast<EnumType>(type);
        return "Te" + enumType->name();
    } else if (llvm::isa<PointerType>(type)) {
        return "P" + typeName(llvm::dyn_cast<PointerType>(type)->pointToType);
    } else if (llvm::isa<ReferenceType>(type)) {
        return "R" + typeName(llvm::dyn_cast<ReferenceType>(type)->referenceToType);
    } else if (llvm::isa<RValueReferenceType>(type)) {
        return "O" + typeName(llvm::dyn_cast<RValueReferenceType>(type)->referenceToType);
    } else if (llvm::isa<ConstType>(type)) {
        return "K" + typeName(llvm::dyn_cast<ConstType>(type)->pointToType);
    } else if (llvm::isa<MutType>(type)) {
        return "Umut" + typeName(llvm::dyn_cast<MutType>(type)->pointToType);
    } else if (llvm::isa<ImmutType>(type)) {
        return "Uimmut" + typeName(llvm::dyn_cast<ImmutType>(type)->pointToType);
    } else {
        std::cerr << "[INTERNAL NAME MANGLING ERROR] type `" << type->getString() << "` not supported!" << std::endl;
        std::exit(1);
    }
    return "[ERROR]";
}

std::string ItaniumMangler::sourceName(const std::string& s) {
    return std::to_string(s.length()) + s;
}
