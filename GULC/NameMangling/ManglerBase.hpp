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

#ifndef GULC_MANGLERBASE_HPP
#define GULC_MANGLERBASE_HPP

#include <string>
#include <AST/Decls/FunctionDecl.hpp>
#include <AST/Decls/GlobalVariableDecl.hpp>
#include <AST/Decls/NamespaceDecl.hpp>
#include <AST/Decls/TemplateFunctionDecl.hpp>
#include <AST/Decls/StructDecl.hpp>

namespace gulc {
    class ManglerBase {
    public:
        virtual void mangle(FunctionDecl* functionDecl) = 0;
        virtual void mangle(GlobalVariableDecl* globalVariableDecl) = 0;
        virtual void mangle(NamespaceDecl* namespaceDecl) = 0;
        virtual void mangle(StructDecl* structDecl) = 0;
        virtual void mangle(TemplateFunctionDecl* templateFunctionDecl) = 0;

        virtual ~ManglerBase() = default;
    };
}

#endif //GULC_MANGLERBASE_HPP
