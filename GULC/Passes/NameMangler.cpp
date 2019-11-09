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

#include <AST/Decls/TemplateFunctionDecl.hpp>
#include <AST/Decls/StructDecl.hpp>
#include "NameMangler.hpp"

using namespace gulc;

void NameMangler::processFile(std::vector<FileAST*>& files) {
    // Prepass to mangle declared type names
    for (FileAST* fileAst : files) {
        for (Decl *decl : fileAst->topLevelDecls()) {
            processTypeDecl(decl);
        }
    }

    for (FileAST* fileAst : files) {
        for (Decl *decl : fileAst->topLevelDecls()) {
            processDecl(decl);
        }
    }
}

void NameMangler::processTypeDecl(Decl *decl) {
    switch (decl->getDeclKind()) {
        case Decl::Kind::Enum:
            _manglerBase->mangleDecl(llvm::dyn_cast<EnumDecl>(decl));
            break;
        case Decl::Kind::Namespace:
            _manglerBase->mangleDecl(llvm::dyn_cast<NamespaceDecl>(decl));
            break;
        case Decl::Kind::Struct:
            _manglerBase->mangleDecl(llvm::dyn_cast<StructDecl>(decl));
            break;
    }
}

void NameMangler::processDecl(Decl *decl) {
    switch (decl->getDeclKind()) {
        case Decl::Kind::Function: {
            auto function = llvm::dyn_cast<FunctionDecl>(decl);
            _manglerBase->mangle(function);
            break;
        }
        case Decl::Kind::GlobalVariable: {
            auto globalVariable = llvm::dyn_cast<GlobalVariableDecl>(decl);
            _manglerBase->mangle(globalVariable);
            break;
        }
        case Decl::Kind::Namespace: {
            auto namespaceDecl = llvm::dyn_cast<NamespaceDecl>(decl);
            _manglerBase->mangle(namespaceDecl);
            break;
        }
        case Decl::Kind::Struct: {
            auto structDecl = llvm::dyn_cast<StructDecl>(decl);
            _manglerBase->mangle(structDecl);
            break;
        }
        case Decl::Kind::TemplateFunction: {
            auto templateFunctionDecl = llvm::dyn_cast<TemplateFunctionDecl>(decl);
            _manglerBase->mangle(templateFunctionDecl);
            break;
        }
    }
}
