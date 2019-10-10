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

#include "NameMangler.hpp"

using namespace gulc;

void NameMangler::processFile(FileAST &fileAst) {
    for (Decl* decl : fileAst.topLevelDecls()) {
        processDecl(decl);
    }
}

void NameMangler::processDecl(Decl *decl) {
    switch (decl->getDeclKind()) {
        case Decl::Kind::Function: {
            auto function = llvm::dyn_cast<FunctionDecl>(decl);
            function->setMangledName(_manglerBase->mangle(function));
            break;
        }
        case Decl::Kind::GlobalVariable: {
            auto globalVariable = llvm::dyn_cast<GlobalVariableDecl>(decl);
            globalVariable->setMangledName(_manglerBase->mangle(globalVariable));
            break;
        }
    }
}
