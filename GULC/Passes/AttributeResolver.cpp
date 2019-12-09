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

#include <AST/Attrs/UnresolvedAttr.hpp>
#include <AST/Attrs/CopyAttr.hpp>
#include <AST/Attrs/MoveAttr.hpp>
#include <AST/Attrs/PodAttr.hpp>
#include "AttributeResolver.hpp"

using namespace gulc;

void AttributeResolver::processFile(std::vector<FileAST *> &files) {
    for (FileAST* fileAst : files) {
        currentFileAst = fileAst;

        processImports(&fileAst->imports());

        for (Decl* decl : fileAst->topLevelDecls()) {
            processDecl(decl);
        }
    }
}

void AttributeResolver::printError(const std::string &message, TextPosition startPosition, TextPosition endPosition) {
    std::cout << "gulc resolver error[" << currentFileAst->filePath() << ", "
                                     "{" << startPosition.line << ", " << startPosition.column << "} "
                                     "to {" << endPosition.line << ", " << endPosition.column << "}]: "
              << message
              << std::endl;
    std::exit(1);
}

void AttributeResolver::printWarning(const std::string &message, TextPosition startPosition, TextPosition endPosition) {
    std::cout << "gulc warning[" << currentFileAst->filePath() << ", "
                              "{" << startPosition.line << ", " << startPosition.column << "} "
                              "to {" << endPosition.line << ", " << endPosition.column << "}]: "
              << message
              << std::endl;
}

void AttributeResolver::processImports(std::vector<Import*>* imports) {
    currentImports = imports;

    if (imports != nullptr) {
        for (Import* checkImport : *imports) {
            for (NamespaceDecl* checkNamespace : _namespacePrototypes) {
                if (checkImport->namespacePath()[0] == checkNamespace->name()) {
                    checkImport->pointToNamespace = validateImportPath(checkNamespace, checkImport->namespacePath(), 1);
                    break;
                }
            }

            if (checkImport->pointToNamespace == nullptr) {
                printError("namespace '" + checkImport->namespacePath()[0] + "' was not found!", {}, {});
            }
        }
    }
}

/// Errors if the import path is invalid...
NamespaceDecl* AttributeResolver::validateImportPath(NamespaceDecl *checkNamespace, const std::vector<std::string> &checkPath,
                                                     std::size_t currentPathIndex) {
    // If the current path index is greater than or equal to the size then we return, the namespace path is valid...
    if (currentPathIndex >= checkPath.size()) {
        // We return the final namespace which will be the namespace the checkPath points to...
        return checkNamespace;
    }

    const std::string& findName = checkPath[currentPathIndex];

    for (Decl* checkDecl : checkNamespace->nestedDecls()) {
        if (llvm::isa<NamespaceDecl>(checkDecl)) {
            auto checkNestedNamespace = llvm::dyn_cast<NamespaceDecl>(checkDecl);

            // If we find the namespace path then we recursively continue checking the path
            if (checkNestedNamespace->name() == findName) {
                return validateImportPath(checkNestedNamespace, checkPath, currentPathIndex + 1);
            }
        }
    }

    // If we reach this point the namespace path was not found...
    std::string currentValidPath = checkPath[0];

    for (std::size_t i = 1; i < currentPathIndex - 1; ++i) {
        currentValidPath += "." + checkPath[i];
    }

    // TODO: We should probably store the start and end for every parsed import...
    printError("namespace identifier '" + checkPath[currentPathIndex] + "' was not found in namespace '" + currentValidPath + "'!", {}, {});
    return nullptr;
}

bool AttributeResolver::resolveAttribute(Attr*& attribute) {
    if (llvm::isa<UnresolvedAttr>(attribute)) {
        auto unresolvedAttr = llvm::dyn_cast<UnresolvedAttr>(attribute);

        // TODO:
    } else if (llvm::isa<CopyAttr>(attribute) ||
               llvm::isa<MoveAttr>(attribute) ||
               llvm::isa<PodAttr>(attribute)) {
        // `Move`, `Copy`, and `POD` are all handled before this point.
        return true;
    }

    return false;
}

void AttributeResolver::processDecl(Decl *decl) {
    switch (decl->getDeclKind()) {
        case Decl::Kind::Function:
            processFunctionDecl(llvm::dyn_cast<FunctionDecl>(decl));
            break;
        case Decl::Kind::GlobalVariable:
            processGlobalVariableDecl(llvm::dyn_cast<GlobalVariableDecl>(decl));
            break;
        case Decl::Kind::Enum:
            processEnumDecl(llvm::dyn_cast<EnumDecl>(decl));
            break;
        case Decl::Kind::Namespace:
            processNamespaceDecl(llvm::dyn_cast<NamespaceDecl>(decl));
            break;
        case Decl::Kind::Struct:
            processStructDecl(llvm::dyn_cast<StructDecl>(decl));
            break;
        case Decl::Kind::TemplateFunction:
            processTemplateFunctionDecl(llvm::dyn_cast<TemplateFunctionDecl>(decl));
            break;
        case Decl::Kind::Parameter:
        case Decl::Kind::TemplateParameter:
        default:
            printWarning("unhandled Decl in 'processDecl'!", decl->startPosition(), decl->endPosition());
            break;
    }
}

// Decls
void AttributeResolver::processConstructorDecl(ConstructorDecl *constructorDecl) {

}

void AttributeResolver::processDestructorDecl(DestructorDecl *destructorDecl) {

}

void AttributeResolver::processEnumDecl(EnumDecl *enumDecl) {

}

void AttributeResolver::processFunctionDecl(FunctionDecl *functionDecl) {

}

void AttributeResolver::processGlobalVariableDecl(GlobalVariableDecl *globalVariableDecl) {

}

void AttributeResolver::processNamespaceDecl(NamespaceDecl *namespaceDecl) {

}

void AttributeResolver::processStructDecl(StructDecl *structDecl) {

}

void AttributeResolver::processTemplateFunctionDecl(TemplateFunctionDecl *templateFunctionDecl) {

}
