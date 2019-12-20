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
#include <AST/Decls/EnumDecl.hpp>
#include <AST/Decls/OperatorDecl.hpp>
#include <AST/Decls/CastOperatorDecl.hpp>
#include <AST/Decls/CallOperatorDecl.hpp>
#include <AST/Decls/IndexOperatorDecl.hpp>

namespace gulc {
    class ManglerBase {
    public:
        // We have to do a prepass on declared types to mangle their names because we will need to access them as
        // parameters in the function signature
        virtual void mangleDecl(EnumDecl* enumDecl) = 0;
        virtual void mangleDecl(StructDecl* structDecl) = 0;
        virtual void mangleDecl(NamespaceDecl* namespaceDecl) = 0;

        virtual void mangle(FunctionDecl* functionDecl) = 0;
        virtual void mangle(GlobalVariableDecl* globalVariableDecl) = 0;
        virtual void mangle(NamespaceDecl* namespaceDecl) = 0;
        virtual void mangle(StructDecl* structDecl) = 0;
        virtual void mangle(TemplateFunctionDecl* templateFunctionDecl) = 0;
        virtual void mangle(OperatorDecl* operatorDecl) = 0;
        virtual void mangle(CastOperatorDecl* castOperatorDecl) = 0;
        virtual void mangle(CallOperatorDecl* callOperatorDecl) = 0;
        virtual void mangle(IndexOperatorDecl* indexOperatorDecl) = 0;

        virtual ~ManglerBase() = default;
    };
}

#endif //GULC_MANGLERBASE_HPP
