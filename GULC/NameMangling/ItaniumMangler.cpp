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
#include <AST/Types/BuiltInType.hpp>
#include <iostream>
#include <AST/Types/EnumType.hpp>
#include <AST/Types/StructType.hpp>
#include "ItaniumMangler.hpp"

using namespace gulc;
//https://itanium-cxx-abi.github.io/cxx-abi/abi.html
// TODO: If we every want to allow `extern` to a `C++` function we will need to support substitution.
//  Even in the areas where substitution makes the result function longer than it would be without substitution using clang v6 and gcc v7.4.0

void ItaniumMangler::mangleDecl(EnumDecl *enumDecl) {
    mangleDeclEnum(enumDecl, "", "");
}

void ItaniumMangler::mangleDecl(StructDecl *structDecl) {
    mangleDeclStruct(structDecl, "", "");
}

void ItaniumMangler::mangleDecl(NamespaceDecl *namespaceDecl) {
    mangleDeclNamespace(namespaceDecl, "");
}

void ItaniumMangler::mangleDeclEnum(EnumDecl *enumDecl, const std::string &prefix, const std::string &nameSuffix) {
    std::string nPrefix = prefix + sourceName(enumDecl->name());
    enumDecl->setMangledName(nPrefix + nameSuffix);
}

void ItaniumMangler::mangleDeclStruct(StructDecl *structDecl, const std::string &prefix, const std::string &nameSuffix) {
    std::string nPrefix = prefix + sourceName(structDecl->name());
    structDecl->setMangledName(nPrefix + nameSuffix);
}

void ItaniumMangler::mangleDeclNamespace(NamespaceDecl *namespaceDecl, const std::string &prefix) {
    std::string nPrefix = prefix + sourceName(namespaceDecl->name());

    for (Decl* decl : namespaceDecl->nestedDecls()) {
        if (llvm::isa<EnumDecl>(decl)) {
            mangleDeclEnum(llvm::dyn_cast<EnumDecl>(decl), "N" + nPrefix, "E");
        } else if (llvm::isa<NamespaceDecl>(decl)) {
            mangleDeclNamespace(llvm::dyn_cast<NamespaceDecl>(decl), nPrefix);
        } else if (llvm::isa<StructDecl>(decl)) {
            mangleDeclStruct(llvm::dyn_cast<StructDecl>(decl), "N" + nPrefix, "E");
        }
    }
}

void ItaniumMangler::mangle(FunctionDecl *functionDecl) {
    mangleFunction(functionDecl, "", "");
}

void ItaniumMangler::mangle(GlobalVariableDecl *globalVariableDecl) {
    mangleVariable(globalVariableDecl, "", "");
}

void ItaniumMangler::mangle(NamespaceDecl *namespaceDecl) {
    mangleNamespace(namespaceDecl, "");
}

void ItaniumMangler::mangle(TemplateFunctionDecl *templateFunctionDecl) {
    mangleTemplateFunction(templateFunctionDecl, "", "");
}

void ItaniumMangler::mangle(StructDecl *structDecl) {
    mangleStruct(structDecl, "");
}

void ItaniumMangler::mangle(OperatorDecl *operatorDecl) {
    mangleOperator(operatorDecl, "", "");
}

void ItaniumMangler::mangle(CastOperatorDecl *castOperatorDecl) {
    mangleCastOperator(castOperatorDecl, "", "");
}

void ItaniumMangler::mangle(CallOperatorDecl *callOperatorDecl) {
    mangleCallOperator(callOperatorDecl, "", "");
}

void ItaniumMangler::mangle(IndexOperatorDecl *indexOperatorDecl) {
    mangleIndexOperator(indexOperatorDecl, "", "");
}

void ItaniumMangler::mangleFunction(FunctionDecl *functionDecl, const std::string &prefix, const std::string &nameSuffix) {
    // All mangled names start with "_Z"...
    std::string mangledName = "_Z" + prefix + unqualifiedName(functionDecl) + nameSuffix;

    mangledName += bareFunctionType(functionDecl->parameters);

    functionDecl->setMangledName(mangledName);
}

void ItaniumMangler::mangleOperator(gulc::OperatorDecl *operatorDecl, const std::string &prefix, const std::string &nameSuffix){
    // All mangled names start with "_Z"...
    std::string mangledName = "_Z" + prefix + operatorName(operatorDecl->operatorType(),
                                                           operatorDecl->operatorName()) + nameSuffix;

    if (operatorDecl->operatorType() == OperatorType::Postfix) {
        // This is a bit of a hack. Because of C++ differentiating between postfix `++` and prefix `++` by using
        // a throwaway `int` parameter, we make ALL postfix operators have `i` in the name to differentiate
        // between prefix and postfix operators.
        //
        // I hate this but we're shooting to be as compatible with the Itanium spec as possible that way we can
        // at least extern to and from C++ more easily (though our classes won't be compatible since it seems like
        // C++ stores non-functions in the vtable... not doing that. You shouldn't pay for what you don't use.)
        mangledName += "i";
    } else {
        mangledName += bareFunctionType(operatorDecl->parameters);
    }

    operatorDecl->setMangledName(mangledName);
}

void ItaniumMangler::mangleCastOperator(CastOperatorDecl *castOperatorDecl, const std::string &prefix, const std::string &nameSuffix) {
    // NOTE: You CANNOT define both an `implicit` and `explicit` that converts to the same type, as such they are
    //       mangled the same way
    std::string mangledName = "_Z" + prefix + "cv" + nameSuffix;

    mangledName += typeName(castOperatorDecl->resultType);
    mangledName += bareFunctionType(castOperatorDecl->parameters);

    castOperatorDecl->setMangledName(mangledName);
}

void ItaniumMangler::mangleCallOperator(CallOperatorDecl *callOperatorDecl, const std::string &prefix,
                                        const std::string &nameSuffix) {
    std::string mangledName = "_Z" + prefix + "cl" + nameSuffix;

    mangledName += bareFunctionType(callOperatorDecl->parameters);

    callOperatorDecl->setMangledName(mangledName);
}

void ItaniumMangler::mangleIndexOperator(IndexOperatorDecl *indexOperatorDecl, const std::string &prefix,
                                         const std::string &nameSuffix) {
    std::string mangledName = "_Z" + prefix + "ix" + nameSuffix;

    mangledName += bareFunctionType(indexOperatorDecl->parameters);

    indexOperatorDecl->setMangledName(mangledName);
}

void ItaniumMangler::mangleVariable(GlobalVariableDecl *variableDecl, const std::string &prefix, const std::string &nameSuffix) {
    // All mangled names start with "_Z"...
    variableDecl->setMangledName("_Z" + prefix + unqualifiedName(variableDecl) + nameSuffix);
}

void ItaniumMangler::mangleNamespace(NamespaceDecl *namespaceDecl, const std::string &prefix) {
    std::string nPrefix = prefix + sourceName(namespaceDecl->name());

    for (Decl* decl : namespaceDecl->nestedDecls()) {
        if (llvm::isa<FunctionDecl>(decl)) {
            mangleFunction(llvm::dyn_cast<FunctionDecl>(decl), "N" + nPrefix, "E");
        } else if (llvm::isa<GlobalVariableDecl>(decl)) {
            mangleVariable(llvm::dyn_cast<GlobalVariableDecl>(decl), "N" + nPrefix, "E");
        } else if (llvm::isa<NamespaceDecl>(decl)) {
            mangleNamespace(llvm::dyn_cast<NamespaceDecl>(decl), nPrefix);
        } else if (llvm::isa<StructDecl>(decl)) {
            mangleStruct(llvm::dyn_cast<StructDecl>(decl), nPrefix);
        } else if (llvm::isa<TemplateFunctionDecl>(decl)) {
            mangleTemplateFunction(llvm::dyn_cast<TemplateFunctionDecl>(decl), "N" + nPrefix, "E");
        }
    }
}

void ItaniumMangler::mangleStruct(StructDecl *structDecl, const std::string &prefix) {
    std::string nPrefix = prefix + sourceName(structDecl->name());

    for (ConstructorDecl* constructor : structDecl->constructors) {
        mangleConstructor(constructor, "N" + nPrefix, "E");
    }

    for (Decl* decl : structDecl->members) {
        if (llvm::isa<OperatorDecl>(decl)) {
            mangleOperator(llvm::dyn_cast<OperatorDecl>(decl), "N" + nPrefix, "E");
        } else if (llvm::isa<CastOperatorDecl>(decl)) {
            mangleCastOperator(llvm::dyn_cast<CastOperatorDecl>(decl), "N" + nPrefix, "E");
        } else if (llvm::isa<CallOperatorDecl>(decl)) {
            mangleCallOperator(llvm::dyn_cast<CallOperatorDecl>(decl), "N" + nPrefix, "E");
        } else if (llvm::isa<IndexOperatorDecl>(decl)) {
            mangleIndexOperator(llvm::dyn_cast<IndexOperatorDecl>(decl), "N" + nPrefix, "E");
        } else if (llvm::isa<FunctionDecl>(decl)) {
            mangleFunction(llvm::dyn_cast<FunctionDecl>(decl), "N" + nPrefix, "E");
        } else if (llvm::isa<TemplateFunctionDecl>(decl)) {
            mangleTemplateFunction(llvm::dyn_cast<TemplateFunctionDecl>(decl), "N" + nPrefix, "E");
        }
    }

    if (structDecl->destructor != nullptr) {
        mangleDestructor(structDecl->destructor, "N" + nPrefix, "E");
    }

    // Set vtable mangled name
    if (prefix.empty()) {
        structDecl->vtableName = "_ZTVN" + nPrefix + "E";
    } else {
        structDecl->vtableName = "_ZTV" + nPrefix;
    }
}

void ItaniumMangler::mangleTemplateFunction(TemplateFunctionDecl* templateFunctionDecl, const std::string& prefix, const std::string& nameSuffix) {
    std::string nPrefix = "_Z" + prefix;

    for (FunctionDecl* implementedFunction : templateFunctionDecl->implementedFunctions()) {
        std::string templatePrefix = unqualifiedName(implementedFunction);
        std::string templateArgsStr = templateArgs(templateFunctionDecl->templateParameters,
                                                   implementedFunction->templateArguments);
        std::string funcType = bareFunctionType(implementedFunction->parameters);;

        std::string mangledName = nPrefix + templatePrefix;
        mangledName += nameSuffix;
        mangledName += templateArgsStr;
        // NOTE: I can't seem to find where in the Itanium spec it says we do this, but GCC and clang both put the
        //  function result type before the function type on template functions? But not normal functions?
        mangledName += typeName(implementedFunction->resultType);
        mangledName += funcType;

        implementedFunction->setMangledName(mangledName);
    }
}

void ItaniumMangler::mangleConstructor(ConstructorDecl *constructorDecl, const std::string &prefix, const std::string &nameSuffix) {
    // All mangled names start with "_Z"...
    std::string mangledName = "_Z" + prefix + "C2";
    std::string mangledNameVTable = "_Z" + prefix + "C1";

    mangledName += nameSuffix;
    mangledNameVTable += nameSuffix;

    std::string bareFunctionTypeResult = bareFunctionType(constructorDecl->parameters);

    // We only have to use <bare-function-name> since there isn't a namespace yet.
    mangledName += bareFunctionTypeResult;
    mangledNameVTable += bareFunctionTypeResult;

    constructorDecl->setMangledName(mangledName);
    constructorDecl->setMangledNameVTable(mangledNameVTable);
}

void ItaniumMangler::mangleDestructor(DestructorDecl *destructorDecl, const std::string &prefix, const std::string &nameSuffix) {
    // All mangled names start with "_Z"...
    std::string mangledName = "_Z" + prefix;

    mangledName += "D2";

    mangledName += nameSuffix;

    // We only have to use <bare-function-name> since there isn't a namespace yet.
    // NOTE: Destructors cannot have parameters but are considered functions so they have to have the 'v' specifier to
    //  show it doesn't accept any parameters here
    mangledName += "v";

    destructorDecl->setMangledName(mangledName);
}

std::string ItaniumMangler::unqualifiedName(FunctionDecl *functionDecl) {
    return sourceName(functionDecl->name());
}

std::string ItaniumMangler::unqualifiedName(GlobalVariableDecl *globalVariableDecl) {
    return sourceName(globalVariableDecl->name());
}

std::string ItaniumMangler::bareFunctionType(std::vector<ParameterDecl*> &params) {
    std::string result;

    if (params.empty()) {
        return "v";
    }

    for (ParameterDecl* param : params) {
        // Parameters that reference template type parameters have to use the template reference strings `T_` and `T{n}_`
        if (param->typeTemplateParamNumber > 0) {
            if (param->typeTemplateParamNumber - 1 == 0) {
                result += "T_";
            } else {
                result += "T" + std::to_string(param->typeTemplateParamNumber - 2) + "_";
            }

            continue;
        }

        Type* genType = param->type;

        // TODO: Should we ignore `mut` here?
        result += typeName(genType);
    }

    return result;
}

std::string ItaniumMangler::typeName(gulc::Type *type) {
    std::string prefix;

    if (type->qualifier() == TypeQualifier::Const) {
        prefix = "K";
    } else if (type->qualifier() == TypeQualifier::Mut) {
        prefix = "Umut";
    }

    if (llvm::isa<BuiltInType>(type)) {
        auto builtInType = llvm::dyn_cast<BuiltInType>(type);
        const std::string checkName = builtInType->name();

        if (checkName == "void") {
            return prefix + "v";
        } else if (checkName == "bool") {
            return prefix + "b";
        } else {
            return prefix + sourceName(checkName);
        }
    } else if (llvm::isa<EnumType>(type)) {
        auto enumType = llvm::dyn_cast<EnumType>(type);
        // TODO: When do we add 'Te' in front of this?? Neither clang nor gcc seem to do it in my tests
        return prefix + /*"Te" + */enumType->decl()->mangledName();
    } else if (llvm::isa<StructType>(type)) {
        auto structType = llvm::dyn_cast<StructType>(type);
        // TODO: When do we add 'Ts' in front of this?? Neither clang nor gcc seem to do it in my tests
        return prefix + /*"Ts" + */structType->decl()->mangledName();
    } else if (llvm::isa<PointerType>(type)) {
        return prefix + "P" + typeName(llvm::dyn_cast<PointerType>(type)->pointToType);
    } else if (llvm::isa<ReferenceType>(type)) {
        return prefix + "R" + typeName(llvm::dyn_cast<ReferenceType>(type)->referenceToType);
    } else {
        std::cerr << "[INTERNAL NAME MANGLING ERROR] type `" << type->getString() << "` not supported!" << std::endl;
        std::exit(1);
    }
    return "[ERROR]";
}

std::string ItaniumMangler::sourceName(const std::string& s) {
    return std::to_string(s.length()) + s;
}

std::string ItaniumMangler::templateArgs(std::vector<TemplateParameterDecl*>& templateParams,
                                         std::vector<Expr*>& templateArgs) {
    std::string result = "I";

    for (std::size_t i = 0; i < templateParams.size(); ++i) {
        const Expr* templateArgExpr = nullptr;

        if (i >= templateArgs.size()) {
            templateArgExpr = templateParams[i]->defaultArgument();
        } else {
            templateArgExpr = templateArgs[i];
        }

        result += templateArg(templateArgExpr);
    }

    return result + "E";
}

std::string ItaniumMangler::templateArg(const Expr *expr) {
    if (llvm::isa<ResolvedTypeRefExpr>(expr)) {
        auto resolvedType = llvm::dyn_cast<ResolvedTypeRefExpr>(expr);
        return typeName(resolvedType->resolvedType);
    } else if (llvm::isa<IntegerLiteralExpr>(expr) || llvm::isa<FloatLiteralExpr>(expr)) {
        return exprPrimary(expr);
    } else {
        std::cerr << "[INTERNAL NAME MANGLING ERROR] template argument not supported!" << std::endl;
        std::exit(1);
    }
}

std::string ItaniumMangler::exprPrimary(const Expr *expr) {
    if (llvm::isa<IntegerLiteralExpr>(expr)) {
        auto integerLiteral = llvm::dyn_cast<IntegerLiteralExpr>(expr);
        // TODO: If the integer is negative it needs to be lead by an `n`
        // TODO: We need to convert to decimal if number base isn't 10
        return "L" + typeName(integerLiteral->resultType) + integerLiteral->numberString + "E";
    } else if (llvm::isa<FloatLiteralExpr>(expr)) {
        auto floatLiteral = llvm::dyn_cast<FloatLiteralExpr>(expr);
        return "L" + typeName(floatLiteral->resultType) + floatLiteral->numberValue() + "E";
    } else {
        std::cerr << "[INTERNAL NAME MANGLING ERROR] expr-primary not supported!" << std::endl;
        std::exit(1);
    }
}

std::string ItaniumMangler::operatorName(OperatorType operatorType, const std::string &operatorText) {
    if (operatorText == "+") {
        if (operatorType == OperatorType::Prefix) {
            return "ps";
        } else if (operatorType == OperatorType::Infix) {
            return "pl";
        } // Postfix isn't supported for `+`
    } else if (operatorText == "-") {
        if (operatorType == OperatorType::Prefix) {
            return "ng";
        } else if (operatorType == OperatorType::Infix) {
            return "mi";
        } // Postfix isn't supported for `-`
    } else if (operatorText == "&") {
        if (operatorType == OperatorType::Prefix) {
            return "ad";
        } else if (operatorType == OperatorType::Infix) {
            return "an";
        } // Postfix isn't supported for `&`
    } else if (operatorText == "*") {
        if (operatorType == OperatorType::Prefix) {
            return "de";
        } else if (operatorType == OperatorType::Infix) {
            return "ml";
        } // Postfix isn't supported for `*`
    } else if (operatorText == "~") {
        if (operatorType == OperatorType::Prefix) {
            return "co";
        } // Postfix and infix isn't supported for `~`
    } else if (operatorText == "/") {
        if (operatorType == OperatorType::Infix) {
            return "dv";
        } // Prefix and postfix isn't supported for `/`
    } else if (operatorText == "%") {
        if (operatorType == OperatorType::Infix) {
            return "rm";
        } // Prefix and postfix isn't supported for `%`
    } else if (operatorText == "|") {
        if (operatorType == OperatorType::Infix) {
            return "or";
        } // Prefix and postfix isn't supported for `|`
    } else if (operatorText == "^") {
        if (operatorType == OperatorType::Infix) {
            return "eo";
        } // Prefix and postfix isn't supported for `^`
    } else if (operatorText == "<<") {
        if (operatorType == OperatorType::Infix) {
            return "ls";
        } // Prefix and postfix isn't supported for `<<`
    } else if (operatorText == ">>") {
        if (operatorType == OperatorType::Infix) {
            return "rs";
        } // Prefix and postfix isn't supported for `>>`
    } else if (operatorText == "==") {
        if (operatorType == OperatorType::Infix) {
            return "eq";
        } // Prefix and postfix isn't supported for `==`
    } else if (operatorText == "!=") {
        if (operatorType == OperatorType::Infix) {
            return "ne";
        } // Prefix and postfix isn't supported for `!=`
    } else if (operatorText == "<") {
        if (operatorType == OperatorType::Infix) {
            return "lt";
        } // Prefix and postfix isn't supported for `<`
    } else if (operatorText == ">") {
        if (operatorType == OperatorType::Infix) {
            return "gt";
        } // Prefix and postfix isn't supported for `>`
    } else if (operatorText == "<=") {
        if (operatorType == OperatorType::Infix) {
            return "le";
        } // Prefix and postfix isn't supported for `<=`
    } else if (operatorText == ">=") {
        if (operatorType == OperatorType::Infix) {
            return "ge";
        } // Prefix and postfix isn't supported for `>=`
    } else if (operatorText == "<=>") {
        if (operatorType == OperatorType::Infix) {
            return "ss";
        } // Prefix and postfix isn't supported for `<=>`
    } else if (operatorText == "!") {
        if (operatorType == OperatorType::Prefix) {
            return "nt";
        } // Infix and postfix isn't supported for `!`
    } else if (operatorText == "&&") {
        if (operatorType == OperatorType::Infix) {
            return "aa";
        } // Prefix and postfix isn't supported for `&&`
    } else if (operatorText == "||") {
        if (operatorType == OperatorType::Infix) {
            return "oo";
        } // Prefix and postfix isn't supported for `||`
    } else if (operatorText == "^^") {
        if (operatorType == OperatorType::Infix) {
            // NOTE: This is usually defined to `std.math.Pow`
            return "ee";
        } // Prefix and postfix isn't supported for `^^`
    } else if (operatorText == "++") {
        if (operatorType == OperatorType::Prefix || operatorType == OperatorType::Postfix) {
            return "pp";
        } // Infix isn't supported for `++`
    } else if (operatorText == "--") {
        if (operatorType == OperatorType::Prefix || operatorType == OperatorType::Postfix) {
            return "mm";
        } // Infix isn't supported for `--`
    } else if (operatorText == "->*") {
        if (operatorType == OperatorType::Infix) {
            return "pm";
        } // Prefix and postfix isn't supported for `->*`
    } else if (operatorText == "->") {
        if (operatorType == OperatorType::Infix) {
            return "pt";
        } // Prefix and postfix isn't supported for `->`
    } else if (operatorText == "()") {
        if (operatorType == OperatorType::Postfix) {
            return "cl";
        } // Prefix and infix isn't supported for `()`
    } else if (operatorText == "[]") {
        if (operatorType == OperatorType::Postfix) {
            return "ix";
        } // Prefix and infix isn't supported for `[]`
    }

    // TODO: There are still some operators we might have missed...
    // Anything that isn't a built in operator is passed to the "vendor extended operator" syntax
    return "v0" + sourceName(operatorText);
}
