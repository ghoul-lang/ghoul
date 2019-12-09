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

#ifndef GULC_ATTRIBUTERESOLVER_HPP
#define GULC_ATTRIBUTERESOLVER_HPP

#include <Targets/Target.hpp>
#include <vector>
#include <AST/Decls/NamespaceDecl.hpp>
#include <AST/FileAST.hpp>
#include <AST/Decls/FunctionDecl.hpp>
#include <AST/Decls/GlobalVariableDecl.hpp>
#include <AST/Decls/EnumDecl.hpp>
#include <AST/Decls/TemplateFunctionDecl.hpp>
#include <AST/Decls/ConstructorDecl.hpp>
#include <AST/Decls/DestructorDecl.hpp>
#include <AST/Decls/StructDecl.hpp>

namespace gulc {
    class AttributeResolver {
    private:
        Target* _target;
        std::vector<NamespaceDecl*>& _namespacePrototypes;
        FileAST* currentFileAst;
        std::vector<Import*>* currentImports;

    public:
        AttributeResolver(Target* target, std::vector<NamespaceDecl*>& namespacePrototypes)
                : _target(target), _namespacePrototypes(namespacePrototypes), currentFileAst(nullptr),
                  currentImports(nullptr) {}

        void processFile(std::vector<FileAST*>& files);

    private:
        void printError(const std::string& message, TextPosition startPosition, TextPosition endPosition);
        void printWarning(const std::string& message, TextPosition startPosition, TextPosition endPosition);

        void processImports(std::vector<Import*>* imports);
        NamespaceDecl* validateImportPath(NamespaceDecl* checkNamespace, const std::vector<std::string>& checkPath,
                                          std::size_t currentPathIndex);

        bool resolveAttribute(Attr*& attribute);

        void processDecl(Decl* decl);

        // Decls
        void processConstructorDecl(ConstructorDecl* constructorDecl);
        void processDestructorDecl(DestructorDecl* destructorDecl);
        void processEnumDecl(EnumDecl* enumDecl);
        void processFunctionDecl(FunctionDecl* functionDecl);
        void processGlobalVariableDecl(GlobalVariableDecl* globalVariableDecl);
        void processNamespaceDecl(NamespaceDecl* namespaceDecl);
        void processStructDecl(StructDecl* structDecl);
        void processTemplateFunctionDecl(TemplateFunctionDecl* templateFunctionDecl);

    };
}

#endif //GULC_ATTRIBUTERESOLVER_HPP
