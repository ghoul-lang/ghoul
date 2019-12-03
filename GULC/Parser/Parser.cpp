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

#include <fstream>
#include <iostream>
#include <sstream>
#include <AST/Types/UnresolvedType.hpp>
#include <AST/Decls/FunctionDecl.hpp>
#include <AST/Exprs/TernaryExpr.hpp>
#include <AST/Exprs/BinaryOperatorExpr.hpp>
#include <AST/Exprs/PrefixOperatorExpr.hpp>
#include <AST/Exprs/PostfixOperatorExpr.hpp>
#include <AST/Exprs/FunctionCallExpr.hpp>
#include <AST/Exprs/IndexerCallExpr.hpp>
#include <AST/Exprs/MemberAccessCallExpr.hpp>
#include <AST/Exprs/IntegerLiteralExpr.hpp>
#include <AST/Exprs/StringLiteralExpr.hpp>
#include <AST/Exprs/CharacterLiteralExpr.hpp>
#include <AST/Exprs/ParenExpr.hpp>
#include <AST/Exprs/FloatLiteralExpr.hpp>
#include <AST/Exprs/PotentialExplicitCastExpr.hpp>
#include <AST/Types/TemplateTypenameType.hpp>
#include <AST/Exprs/LocalVariableDeclOrPrefixOperatorCallExpr.hpp>
#include <AST/Stmts/LabeledStmt.hpp>
#include <AST/Exprs/UnresolvedTypeRefExpr.hpp>
#include <AST/Exprs/LocalVariableDeclExpr.hpp>
#include <AST/Types/PointerType.hpp>
#include <AST/Types/ReferenceType.hpp>
#include <AST/Decls/GlobalVariableDecl.hpp>
#include <AST/Decls/EnumConstantDecl.hpp>
#include <AST/Decls/EnumDecl.hpp>
#include <AST/Decls/NamespaceDecl.hpp>
#include <AST/Decls/TemplateFunctionDecl.hpp>
#include <AST/Decls/StructDecl.hpp>
#include <AST/Decls/ConstructorDecl.hpp>
#include "Parser.hpp"

using namespace gulc;

Parser::Parser(std::string filePath)
        : _filePath(std::move(filePath)) {
    std::ifstream fileStream(_filePath);

    if (fileStream.good()) {
        std::stringstream buffer;
        buffer << fileStream.rdbuf();

        _lexer = Lexer(_filePath, buffer.str());
    } else {
        std::cout << "gulc error: file '" << _filePath << "' was not found!" << std::endl;
        std::exit(1);
    }
}

/**
 * Parse the source code file into a `FileAST` object
 *
 * @return parsed in file AST with information on the file
 */
FileAST Parser::parseFile() {
    FileAST result(_filePath);

    while (_lexer.peekType() != TokenType::ENDOFFILE) {
        if (_lexer.peekType() == TokenType::IMPORT) {
            _lexer.consumeType(TokenType::IMPORT);

            std::vector<std::string> namespacePath{};
            namespacePath.push_back(_lexer.peekToken().currentSymbol);

            if (!_lexer.consumeType(TokenType::SYMBOL)) {
                printError("expected namespace path, found '" + _lexer.peekToken().currentSymbol + "'!",
                           _lexer.peekToken().startPosition, _lexer.peekToken().endPosition);
                continue;
            }

            while (_lexer.peekType() != TokenType::SEMICOLON && _lexer.peekType() != TokenType::AS) {
                if (!_lexer.consumeType(TokenType::PERIOD)) {
                    printError("expected namespace path separator '.', found '" + _lexer.peekToken().currentSymbol + "'!",
                               _lexer.peekToken().startPosition, _lexer.peekToken().endPosition);
                    break;
                }

                namespacePath.push_back(_lexer.peekToken().currentSymbol);

                if (!_lexer.consumeType(TokenType::SYMBOL)) {
                    printError("expected namespace path, found '" + _lexer.peekToken().currentSymbol + "'!",
                               _lexer.peekToken().startPosition, _lexer.peekToken().endPosition);
                    continue;
                }
            }

            std::string alias;

            if (_lexer.consumeType(TokenType::AS)) {
                alias = _lexer.peekToken().currentSymbol;

                if (!_lexer.consumeType(TokenType::SYMBOL)) {
                    printError("expected import alias, found '" + _lexer.peekToken().currentSymbol + "'!",
                               _lexer.peekToken().startPosition, _lexer.peekToken().endPosition);
                }
            }

            if (!_lexer.consumeType(TokenType::SEMICOLON)) {
                printError("expected ';' after import, found '" + _lexer.peekToken().currentSymbol + "'!",
                           _lexer.peekToken().startPosition, _lexer.peekToken().endPosition);
            }

            result.addImport(new Import(namespacePath, alias));
        } else {
            result.addTopLevelDecl(parseTopLevelDecl());
        }
    }

    return result;
}

/**
 * Print an error to the console with the position in the source code file that we are erroring out for
 * After print the error message out we exit the application with exit code `1`
 *
 * @param errorMessage - message to print to the console
 * @param startPosition - start position of the section of code that caused the error
 * @param endPosition - end position of the section of code that caused the error
 */
void Parser::printError(const std::string& errorMessage, TextPosition startPosition, TextPosition endPosition) {
    std::cout << "gulc parser error[" << _filePath << ", "
                                   "{" << startPosition.line << ", " << startPosition.column << "} "
                                   "to {" << endPosition.line << ", " << endPosition.column << "}]: "
              << errorMessage
              << std::endl;

    std::exit(1);
}

/**
 * Print a warning to the console with the position in the source code file that we are warning about
 *
 * @param warningMessage - message to print to the console
 * @param startPosition - start position of the section of code to warn about
 * @param endPosition - end position of the section of code to warn about
 */
void Parser::printWarning(const std::string &warningMessage, TextPosition startPosition, TextPosition endPosition) {
    std::cout << "gulc parser warning[" << _filePath << ", "
                                     "{" << startPosition.line << ", " << startPosition.column << "} "
                                     "to {" << endPosition.line << ", " << endPosition.column << "}]: "
              << warningMessage
              << std::endl;
}

/**
 * This parses a 'top-level' declaration which is a non-member function, non-member variable, namespace,
 * class, struct, enum, interface, union, etc.
 *
 * @return Based on source any of {
 *     FunctionDecl,
 *     NamespaceDecl,
 *     VariableDecl,
 *     ClassDecl,
 *     StructDecl,
 *     EnumDecl,
 *     InterfaceDecl,
 *     UnionDecl
 * }
 */
Decl *Parser::parseTopLevelDecl() {
    Token peekedToken = _lexer.peekToken();
    TextPosition startPosition = peekedToken.startPosition;
    TextPosition endPosition = peekedToken.endPosition;

    bool isExtern = false;
    bool isVolatile = false;
    bool isAbstract = false;
    bool isSealed = false;
    bool isVirtual = false;
    bool isOverride = false;
    /*
		IN, // 'in'
		OUT, // 'out'
		REF, // 'ref'
     */
    Decl::Visibility visibility = Decl::Visibility::Unspecified;

    while (peekedToken.metaType == TokenMetaType::MODIFIER) {
        switch (peekedToken.tokenType) {
            case TokenType::PUBLIC:
                switch (visibility) {
                    case Decl::Visibility::Unspecified:
                        visibility = Decl::Visibility::Public;
                        break;
                    case Decl::Visibility::Public:
                        printWarning("duplicate `public` specifier!",
                                     peekedToken.startPosition, peekedToken.endPosition);
                        break;
                    case Decl::Visibility::Private:
                        printError("cannot specify both `private` and `public` at the same time!",
                                   peekedToken.startPosition, peekedToken.endPosition);
                        break;
                    case Decl::Visibility::Protected:
                        printError("cannot specify both `protected` and `public` at the same time!",
                                   peekedToken.startPosition, peekedToken.endPosition);
                        break;
                    case Decl::Visibility::Internal:
                        printError("cannot specify both `internal` and `public` at the same time!",
                                   peekedToken.startPosition, peekedToken.endPosition);
                        break;
                    case Decl::Visibility::ProtectedInternal:
                        printError("cannot specify both `protected internal` and `public` at the same time!",
                                   peekedToken.startPosition, peekedToken.endPosition);
                        break;
                }
                _lexer.consumeType(TokenType::PUBLIC);
                break;
            case TokenType::PRIVATE:
                switch (visibility) {
                    case Decl::Visibility::Unspecified:
                        visibility = Decl::Visibility::Private;
                        break;
                    case Decl::Visibility::Public:
                        printError("cannot specify both `public` and `private` at the same time!",
                                   peekedToken.startPosition, peekedToken.endPosition);
                        break;
                    case Decl::Visibility::Private:
                        printWarning("duplicate `private` specifier!",
                                     peekedToken.startPosition, peekedToken.endPosition);
                        break;
                    case Decl::Visibility::Protected:
                        printError("cannot specify both `protected` and `private` at the same time!",
                                   peekedToken.startPosition, peekedToken.endPosition);
                        break;
                    case Decl::Visibility::Internal:
                        printError("cannot specify both `internal` and `private` at the same time!",
                                   peekedToken.startPosition, peekedToken.endPosition);
                        break;
                    case Decl::Visibility::ProtectedInternal:
                        printError("cannot specify both `protected internal` and `private` at the same time!",
                                   peekedToken.startPosition, peekedToken.endPosition);
                        break;
                }
                _lexer.consumeType(TokenType::PRIVATE);
                break;
            case TokenType::PROTECTED:
                switch (visibility) {
                    case Decl::Visibility::Unspecified:
                        visibility = Decl::Visibility::Protected;
                        break;
                    case Decl::Visibility::Public:
                        printError("cannot specify both `public` and `protected` at the same time!",
                                   peekedToken.startPosition, peekedToken.endPosition);
                        break;
                    case Decl::Visibility::Private:
                        printError("cannot specify both `private` and `protected` at the same time!",
                                   peekedToken.startPosition, peekedToken.endPosition);
                        break;
                    case Decl::Visibility::Protected:
                        printWarning("duplicate `protected` specifier!",
                                     peekedToken.startPosition, peekedToken.endPosition);
                        break;
                    case Decl::Visibility::Internal:
                        printWarning("`internal protected` is out of order, replace with `protected internal`!",
                                     peekedToken.startPosition, peekedToken.endPosition);
                        visibility = Decl::Visibility::ProtectedInternal;
                        break;
                    case Decl::Visibility::ProtectedInternal:
                        printError("cannot specify both `protected internal` and `protected` at the same time!",
                                   peekedToken.startPosition, peekedToken.endPosition);
                        break;
                }
                _lexer.consumeType(TokenType::PROTECTED);
                break;
            case TokenType::INTERNAL:
                switch (visibility) {
                    case Decl::Visibility::Unspecified:
                        visibility = Decl::Visibility::Internal;
                        break;
                    case Decl::Visibility::Public:
                        printError("cannot specify both `public` and `internal` at the same time!",
                                   peekedToken.startPosition, peekedToken.endPosition);
                        break;
                    case Decl::Visibility::Private:
                        printError("cannot specify both `private` and `internal` at the same time!",
                                   peekedToken.startPosition, peekedToken.endPosition);
                        break;
                    case Decl::Visibility::Protected:
                        visibility = Decl::Visibility::ProtectedInternal;
                        break;
                    case Decl::Visibility::Internal:
                        printWarning("duplicate `internal` specifier!",
                                     peekedToken.startPosition, peekedToken.endPosition);
                        break;
                    case Decl::Visibility::ProtectedInternal:
                        printError("cannot specify both `protected internal` and `internal` at the same time!",
                                   peekedToken.startPosition, peekedToken.endPosition);
                        break;
                }
                _lexer.consumeType(TokenType::INTERNAL);
                break;
            case TokenType::STATIC:
                printError("'static' cannot be applied to top-level declarations!", peekedToken.startPosition, peekedToken.endPosition);
                return nullptr;
            case TokenType::VIRTUAL:
                if (isVirtual) {
                    printWarning("duplicate 'virtual' specifier", peekedToken.startPosition, peekedToken.endPosition);
                }

                if (isAbstract) printError("'virtual' and 'abstract' cannot be used at the same time!", peekedToken.startPosition, peekedToken.endPosition);
                if (isOverride) printError("'virtual' and 'override' cannot be used at the same time!", peekedToken.startPosition, peekedToken.endPosition);

                _lexer.consumeType(TokenType::VIRTUAL);
                isVirtual = true;
                break;
            case TokenType::OVERRIDE:
                if (isOverride) {
                    printWarning("duplicate 'override' specifier", peekedToken.startPosition, peekedToken.endPosition);
                }

                if (isAbstract) printError("'override' and 'abstract' cannot be used at the same time!", peekedToken.startPosition, peekedToken.endPosition);
                if (isVirtual) printError("'override' and 'virtual' cannot be used at the same time!", peekedToken.startPosition, peekedToken.endPosition);

                _lexer.consumeType(TokenType::OVERRIDE);
                isOverride = true;
                break;
            case TokenType::EXTERN:
                if (isExtern) printWarning("duplicate 'extern' specifier", peekedToken.startPosition, peekedToken.endPosition);

                _lexer.consumeType(TokenType::EXTERN);
                isExtern = true;
                break;
            case TokenType::VOLATILE:
                if (isVolatile) {
                    printWarning("duplicate 'volatile' specifier", peekedToken.startPosition, peekedToken.endPosition);
                }

                _lexer.consumeType(TokenType::VOLATILE);
                isVolatile = true;
                break;
            case TokenType::ABSTRACT:
                if (isAbstract) {
                    printWarning("duplicate 'abstract' specifier", peekedToken.startPosition, peekedToken.endPosition);
                }

                if (isVirtual) printError("'abstract' and 'virtual' cannot be used at the same time!", peekedToken.startPosition, peekedToken.endPosition);
                if (isOverride) printError("'abstract' and 'override' cannot be used at the same time!", peekedToken.startPosition, peekedToken.endPosition);

                _lexer.consumeType(TokenType::ABSTRACT);
                isAbstract = true;
                break;
            case TokenType::SEALED:
                if (isSealed) printWarning("duplicate 'sealed' specifier", peekedToken.startPosition, peekedToken.endPosition);

                _lexer.consumeType(TokenType::SEALED);
                isSealed = true;
                break;
            case TokenType::CONST:
            case TokenType::MUT:
                // We break from the loop if the token is `const` or `mut`
                goto qualifierFound;
            default:
                printError("unknown qualifier '" + peekedToken.currentSymbol + "'!",
                           peekedToken.startPosition, peekedToken.endPosition);
                return nullptr;
        }

        peekedToken = _lexer.peekToken();
        endPosition = peekedToken.endPosition;
    }

qualifierFound:

    switch (peekedToken.tokenType) {
        case TokenType::NAMESPACE: {
            _lexer.consumeType(TokenType::NAMESPACE);

            if (visibility != Decl::Visibility::Unspecified) {
                printError("namespaces cannot have a visibility modifier!",
                           startPosition, _lexer.peekToken().endPosition);
            }

            std::string name = _lexer.peekToken().currentSymbol;

            endPosition = _lexer.peekToken().endPosition;

            if (!_lexer.consumeType(TokenType::SYMBOL)) {
                printError("expected namespace name, found '" + name + "'!",
                           _lexer.peekToken().startPosition, _lexer.peekToken().endPosition);
                return nullptr;
            }

            auto resultNamespace = new NamespaceDecl(name, _filePath, startPosition, endPosition);
            auto workingNamespace = resultNamespace;

            while (_lexer.peekType() == TokenType::PERIOD) {
                _lexer.consumeType(TokenType::PERIOD);

                TextPosition namespaceStartPosition = _lexer.peekToken().startPosition;
                endPosition = _lexer.peekToken().endPosition;

                std::string nestedName = _lexer.peekToken().currentSymbol;

                if (!_lexer.consumeType(TokenType::SYMBOL)) {
                    printError("expected namespace name after `.`, found '" + nestedName + "'!",
                               _lexer.peekToken().startPosition, _lexer.peekToken().endPosition);
                    return nullptr;
                }

                auto newWorkingNamespace = new NamespaceDecl(nestedName, _filePath,
                                                             namespaceStartPosition, endPosition);

                // We nest the new working namespace into the current one...
                workingNamespace->addNestedDecl(newWorkingNamespace);
                // Then set the current working namespace to be the new one
                // This makes it so `namespace std.io` -> `namespace std { namespace io {} }`
                workingNamespace = newWorkingNamespace;
            }

            if (!_lexer.consumeType(TokenType::LCURLY)) {
                printError("expected namespace beginning `{`, found '" + _lexer.peekToken().currentSymbol + "'!",
                           _lexer.peekToken().startPosition, _lexer.peekToken().endPosition);
                return nullptr;
            }

            while (_lexer.peekType() != TokenType::RCURLY &&
                   _lexer.peekType() != TokenType::ENDOFFILE) {
                workingNamespace->addNestedDecl(parseTopLevelDecl());
            }

            if (!_lexer.consumeType(TokenType::RCURLY)) {
                printError("expected namespace ending `}`, found '" + _lexer.peekToken().currentSymbol + "'!",
                           _lexer.peekToken().startPosition, _lexer.peekToken().endPosition);
                return nullptr;
            }

            // Return the root namespace...
            return resultNamespace;
        }
        case TokenType::CLASS:
            printError("classes not yet supported!", startPosition, endPosition);
            return nullptr;
        case TokenType::STRUCT: {
            _lexer.consumeType(TokenType::STRUCT);

            std::string name = _lexer.peekToken().currentSymbol;

            if (!_lexer.consumeType(TokenType::SYMBOL)) {
                printError("expected struct name after `struct` keyword, found '" + name + "'!",
                           _lexer.peekToken().startPosition, _lexer.peekToken().endPosition);
                return nullptr;
            }

            std::vector<Type*> baseTypes;

            // Parse base types
            if (_lexer.consumeType(TokenType::COLON)) {
                while (_lexer.peekType() != TokenType::LCURLY && _lexer.peekType() != TokenType::ENDOFFILE) {
                    baseTypes.push_back(parseType(false));

                    // Break if there isn't a comma after the base type we just parsed
                    if (!_lexer.consumeType(TokenType::COMMA)) {
                        break;
                    }
                }
            }

            if (!_lexer.consumeType(TokenType::LCURLY)) {
                printError("expected opening '{' for struct, found '" + _lexer.peekToken().currentSymbol + "'!",
                           _lexer.peekToken().startPosition, _lexer.peekToken().endPosition);
                return nullptr;
            }

            std::vector<ConstructorDecl*> constructors{};
            std::vector<Decl*> members{};
            DestructorDecl* destructor = nullptr;

            while (_lexer.peekType() != TokenType::RCURLY && _lexer.peekType() != TokenType::ENDOFFILE) {
                Decl* topLevelDecl = parseTopLevelDecl();

                if (llvm::isa<ConstructorDecl>(topLevelDecl)) {
                    constructors.push_back(llvm::dyn_cast<ConstructorDecl>(topLevelDecl));
                } else if (llvm::isa<DestructorDecl>(topLevelDecl)) {
                    if (destructor != nullptr) {
                        printError("structs cannot have more than one destructor!",
                                   topLevelDecl->startPosition(), topLevelDecl->endPosition());
                    } else {
                        destructor = llvm::dyn_cast<DestructorDecl>(topLevelDecl);
                    }
                } else {
                    members.push_back(topLevelDecl);
                }
            }

            endPosition = _lexer.peekToken().endPosition;

            if (!_lexer.consumeType(TokenType::RCURLY)) {
                printError("expected closing '}' for struct, found '" + _lexer.peekToken().currentSymbol + "'!",
                           _lexer.peekToken().startPosition, _lexer.peekToken().endPosition);
                return nullptr;
            }

            return new StructDecl(name, _filePath, startPosition, endPosition, visibility,
                                  std::move(baseTypes), std::move(constructors), std::move(members), destructor);
        }
        case TokenType::UNION:
            printError("unions not yet supported!", startPosition, endPosition);
            return nullptr;
        case TokenType::INTERFACE:
            printError("interfaces not yet supported!", startPosition, endPosition);
            return nullptr;
        case TokenType::ENUM: {
            _lexer.consumeType(TokenType::ENUM);

            std::string name = _lexer.peekToken().currentSymbol;

            if (!_lexer.consumeType(TokenType::SYMBOL)) {
                printError("expected enum name after `enum` keyword, found '" + name + "'!",
                           _lexer.peekToken().startPosition, _lexer.peekToken().endPosition);
                return nullptr;
            }

            Type* baseType = nullptr;

            if (_lexer.peekType() == TokenType::COLON) {
                _lexer.consumeType(TokenType::COLON);
                baseType = parseType(true);
            }

            if (!_lexer.consumeType(TokenType::LCURLY)) {
                printError("expected opening '{', found '" + _lexer.peekToken().currentSymbol + "'!",
                           _lexer.peekToken().startPosition, _lexer.peekToken().endPosition);
                return nullptr;
            }

            std::vector<EnumConstantDecl*> constants{};

            while (_lexer.peekType() != TokenType::RCURLY && _lexer.peekType() != TokenType::ENDOFFILE) {
                std::string constantName = _lexer.peekToken().currentSymbol;
                TextPosition constantStartPosition = _lexer.peekToken().startPosition;
                TextPosition constantEndPosition = _lexer.peekToken().endPosition;

                if (!_lexer.consumeType(TokenType::SYMBOL)) {
                    printError("expected enum constant name, found '" + constantName + "'!",
                               constantStartPosition, constantEndPosition);
                    return nullptr;
                }

                Expr* constantValue = nullptr;

                if (_lexer.peekType() == TokenType::EQUALS) {
                    _lexer.consumeType(TokenType::EQUALS);

                    constantValue = parseExpr(false, false);
                }

                constants.push_back(new EnumConstantDecl(constantName, _filePath,
                                                         constantStartPosition, constantEndPosition,
                                                         constantValue));

                if (!_lexer.consumeType(TokenType::COMMA)) break;
            }

            endPosition = _lexer.peekToken().endPosition;

            if (!_lexer.consumeType(TokenType::RCURLY)) {
                printError("expected enum closing '}', found '" + _lexer.peekToken().currentSymbol + "'!",
                           _lexer.peekToken().startPosition, _lexer.peekToken().endPosition);
                return nullptr;
            }

            return new EnumDecl(name, _filePath, startPosition, endPosition, visibility, baseType, constants);
        }
        case TokenType::TILDE: {
            // Parse destructor
            _lexer.consumeType(TokenType::TILDE);

            if (visibility != Decl::Visibility::Unspecified) {
                printError("destructors cannot have a visibility modifier!",
                           startPosition, _lexer.peekToken().endPosition);
            }

            std::string verifyName = _lexer.peekToken().currentSymbol;

            if (!_lexer.consumeType(TokenType::SYMBOL)) {
                printError("expected type name after destructor! (found '" + verifyName + "')",
                           _lexer.peekToken().startPosition, _lexer.peekToken().endPosition);
                return nullptr;
            }

            if (!_lexer.consumeType(TokenType::LPAREN)) {
                printError("expected beginning `(` after destructor type name! (found '" + _lexer.peekToken().currentSymbol + "')",
                           _lexer.peekToken().startPosition, _lexer.peekToken().endPosition);
                return nullptr;
            }

            endPosition = _lexer.peekToken().endPosition;

            if (!_lexer.consumeType(TokenType::RPAREN)) {
                printError("expected ending `)` after destructor type name! (found '" + _lexer.peekToken().currentSymbol + "')",
                           _lexer.peekToken().startPosition, _lexer.peekToken().endPosition);
                return nullptr;
            }

            CompoundStmt* compoundStmt = parseCompoundStmt();

            FunctionModifiers modifier = FunctionModifiers::None;

            if (isVirtual) {
                modifier = FunctionModifiers::Virtual;
            } else if (isAbstract) {
                modifier = FunctionModifiers::Abstract;
            } else if (isOverride) {
                modifier = FunctionModifiers::Override;
            }

            return new DestructorDecl(verifyName, _filePath, startPosition, endPosition, modifier, compoundStmt);
        }
        case TokenType::CONST:
        case TokenType::MUT:
        case TokenType::SYMBOL: {
            Type* resultType = parseType();

            peekedToken = _lexer.peekToken();
            endPosition = peekedToken.endPosition;

            bool parseConstructor = false;
            std::string name;

            // Parse the rest of the type and the name of the variable or function
            switch (peekedToken.tokenType) {
                case TokenType::LPAREN:
                    parseConstructor = true;

                    if (!llvm::isa<UnresolvedType>(resultType)) {
                        printError("unexpected expression where constructor name was expected!",
                                   _lexer.peekToken().startPosition, _lexer.peekToken().endPosition);
                    }

                    // What we parse as the type is actually the constructor name, so we steal the name
                    name = llvm::dyn_cast<UnresolvedType>(resultType)->name();

                    // Delete the old parsed type
                    delete resultType;
                    break;
                case TokenType::SYMBOL:
                    // This is the function or variable declaration name
                    name = peekedToken.currentSymbol;
                    _lexer.consumeType(TokenType::SYMBOL);
                    break;
                default:
                    printError("unexpected token in top-level declaration! (found '" + _lexer.peekToken().currentSymbol + "')",
                               _lexer.peekToken().startPosition, _lexer.peekToken().endPosition);
                    return nullptr;
            }

            peekedToken = _lexer.peekToken();
            endPosition = peekedToken.endPosition;

            // Detect if it is a variable or function and parse based on the detection
            switch (peekedToken.tokenType) {
                case TokenType::LESS: { // Template Function
                    if (parseConstructor) {
                        printError("constructors cannot have template parameters! (or template function is missing a type?)",
                                   _lexer.peekToken().startPosition, _lexer.peekToken().endPosition);
                    }

                    std::vector<TemplateParameterDecl*> templateParameters = parseTemplateParameterDecls(startPosition);
                    std::vector<ParameterDecl*> parameters = parseParameterDecls(startPosition);
                    // TODO: Allow modifiers after the end parenthesis (e.g. 'where T : IArray<?>'
                    CompoundStmt* compoundStmt = parseCompoundStmt();

                    if (isVirtual) {
                        printError("template functions cannot be `virtual`!", startPosition, endPosition);
                    } else if (isAbstract) {
                        printError("template functions cannot be `abstract`!", startPosition, endPosition);
                    } else if (isOverride) {
                        printError("template functions cannot be `override`!", startPosition, endPosition);
                    }

                    return new TemplateFunctionDecl(name, _filePath, startPosition, endPosition, visibility,
                                                    FunctionModifiers::None, resultType, templateParameters,
                                                    parameters, compoundStmt);
                }
                case TokenType::LPAREN: { // Function
                    std::vector<ParameterDecl*> parameters = parseParameterDecls(startPosition);

                    if (parseConstructor) {
                        BaseConstructorCallExpr* baseConstructorCall = nullptr;

                        if (_lexer.consumeType(TokenType::COLON)) {
                            // If there is a colon after the parameters then we have an explicit base constructor call
                            bool isThisCall = false;
                            std::string const& checkSymbol = _lexer.peekToken().currentSymbol;

                            if (checkSymbol == "this") {
                                isThisCall = true;
                            } else if (checkSymbol != "base") {
                                printError("expected base constructor call, found '" + _lexer.peekToken().currentSymbol + "'!",
                                           _lexer.peekToken().startPosition, _lexer.peekToken().endPosition);
                            }

                            TextPosition baseConstructorStartPosition = _lexer.peekToken().startPosition;

                            // Consume the `base` or `this` token
                            _lexer.consumeType(TokenType::SYMBOL);

                            // Parse the base constructor's parameters
                            std::vector<Expr*> baseConstructorArguments;

                            if (!_lexer.consumeType(TokenType::LPAREN)) {
                                printError("expected beginning '(' after 'base', found '" + _lexer.peekToken().currentSymbol + "'!",
                                           _lexer.peekToken().startPosition, _lexer.peekToken().endPosition);
                            }

                            while (_lexer.peekType() != TokenType::RPAREN &&
                                   _lexer.peekType() != TokenType::ENDOFFILE) {
                                // TODO: Should `templateTypingAllowed` be true?
                                baseConstructorArguments.push_back(parseExpr(false, false));

                                // Break if the next token after the argument isn't a comma
                                if (!_lexer.consumeType(TokenType::COMMA)) {
                                    break;
                                }
                            }

                            TextPosition baseConstructorEndPosition = _lexer.peekToken().endPosition;

                            if (!_lexer.consumeType(TokenType::RPAREN)) {
                                printError("expected ending '(' for 'base', found '" + _lexer.peekToken().currentSymbol + "'!",
                                           _lexer.peekToken().startPosition, _lexer.peekToken().endPosition);
                            }

                            baseConstructorCall = new BaseConstructorCallExpr(baseConstructorStartPosition,
                                                                              baseConstructorEndPosition,
                                                                              isThisCall, nullptr,
                                                                              baseConstructorArguments);
                        }

                        // TODO: Should we allow modifiers after the end parenthesis (e.g. 'where T : IArray<?>'
                        CompoundStmt* compoundStmt = parseCompoundStmt();

                        return new ConstructorDecl(name, _filePath, startPosition, endPosition, visibility, parameters,
                                                   baseConstructorCall, compoundStmt);
                    } else {
                        // TODO: Allow modifiers after the end parenthesis (e.g. 'where T : IArray<?>'
                        CompoundStmt* compoundStmt = parseCompoundStmt();

                        FunctionModifiers modifier = FunctionModifiers::None;

                        if (isVirtual) {
                            modifier = FunctionModifiers::Virtual;
                        } else if (isAbstract) {
                            modifier = FunctionModifiers::Abstract;
                        } else if (isOverride) {
                            modifier = FunctionModifiers::Override;
                        }

                        return new FunctionDecl(name, _filePath, startPosition, endPosition, visibility,
                                                modifier, resultType, parameters, compoundStmt);
                    }
                }
                case TokenType::EQUALS: {
                    if (parseConstructor) {
                        printError("expected variable name, found '='!",
                                   _lexer.peekToken().startPosition,
                                   _lexer.peekToken().endPosition);
                    }

                    _lexer.consumeType(TokenType::EQUALS);
                    Expr *initialValue = parseExpr(false, false);

                    endPosition = _lexer.peekToken().endPosition;

                    if (!_lexer.consumeType(TokenType::SEMICOLON)) {
                        printError("expected `;` after global variable declaration!",
                                   _lexer.peekToken().startPosition, _lexer.peekToken().endPosition);
                        return nullptr;
                    }

                    return new GlobalVariableDecl(name, _filePath, startPosition, endPosition, visibility, resultType, initialValue);
                }
                case TokenType::SEMICOLON:
                    if (parseConstructor) {
                        printError("expected variable name, found ';'!",
                                   _lexer.peekToken().startPosition, _lexer.peekToken().endPosition);
                    }

                    endPosition = _lexer.peekToken().endPosition;

                    if (!_lexer.consumeType(TokenType::SEMICOLON)) {
                        printError("expected `;` after global variable declaration!",
                                   _lexer.peekToken().startPosition, _lexer.peekToken().endPosition);
                        return nullptr;
                    }

                    return new GlobalVariableDecl(name, _filePath, startPosition, endPosition, visibility, resultType, nullptr);
                default:
                    printError("unexpected token in top-level declaration! (found '" + _lexer.peekToken().currentSymbol + "')", startPosition, endPosition);
                    return nullptr;
            }
        }
        default:
            printError("unknown top level declaration!", startPosition, endPosition);
            return nullptr;
    }
}

/**
 * Generic function to parse template parameters
 *
 * Can parse templates from:
 * `int funcEx<typename T, int i, lifetime a>(){}`
 * `class box<T>`
 * `struct fixed_array<typename T, size_t length>`
 *
 * @param startPosition
 * @return `std::vector` of parsed `TemplateParameterDecl`s
 */
std::vector<TemplateParameterDecl *> Parser::parseTemplateParameterDecls(TextPosition startPosition) {
    std::vector<TemplateParameterDecl*> result{};

    TextPosition endPosition = _lexer.peekToken().endPosition;

    if (!_lexer.consumeType(TokenType::LESS)) {
        printError("expected start '<' for template data declaration! (found '" + _lexer.peekToken().currentSymbol + "')",
                   startPosition, endPosition);
        return std::vector<TemplateParameterDecl*>();
    }

    bool oldRightShiftEnabledValue = _lexer.getRightShiftState();
    _lexer.setRightShiftState(false);

    for (Token peekedToken = _lexer.peekToken();
         peekedToken.tokenType != TokenType::TEMPLATEEND && peekedToken.tokenType != TokenType::ENDOFFILE;
         peekedToken = _lexer.peekToken()) {
        Type* paramType = parseType();

        peekedToken = _lexer.peekToken();

        std::string paramName;
        Expr* defaultArgument = nullptr;

        if (peekedToken.tokenType == TokenType::COMMA || peekedToken.tokenType == TokenType::EQUALS ||
                                                         peekedToken.tokenType == TokenType::TEMPLATEEND) {
            if (llvm::isa<UnresolvedType>(paramType)) {
                auto* unresolvedType = llvm::dyn_cast<UnresolvedType>(paramType);
                paramName = unresolvedType->name();
                // Since this doesn't actually exist in the source code we just copy the name's position.
                paramType = new TemplateTypenameType(unresolvedType->startPosition(), unresolvedType->endPosition());
                delete unresolvedType;

                if (_lexer.peekType() == TokenType::EQUALS) {
                    _lexer.consumeType(TokenType::EQUALS);
                    // TODO: Is the `isStatement = false` correct?
                    defaultArgument = parsePrefixes(false, false);
                    endPosition = defaultArgument->endPosition();
                }
            } else {
                printError("expected template parameter name! (found '" + _lexer.peekToken().currentSymbol + "')",
                           peekedToken.startPosition, peekedToken.endPosition);
                return std::vector<TemplateParameterDecl *>();
            }
        } else {
            paramName = peekedToken.currentSymbol;
            endPosition = _lexer.peekToken().endPosition;

            if (!_lexer.consumeType(TokenType::SYMBOL)) {
                printError("expected template parameter name! (found '" + _lexer.peekToken().currentSymbol + "')",
                           peekedToken.startPosition, peekedToken.endPosition);
                return std::vector<TemplateParameterDecl *>();
            }

            peekedToken = _lexer.peekToken();

            if (peekedToken.tokenType == TokenType::EQUALS) {
                _lexer.consumeType(TokenType::EQUALS);
                // TODO: Is the `isStatement = false` correct?
                defaultArgument = parsePrefixes(false, false);
                endPosition = defaultArgument->endPosition();
            }
        }

        // Add new 'ParameterDecl' to the result list
        result.push_back(new TemplateParameterDecl(paramName, _filePath, paramType->startPosition(), endPosition,
                                                   paramType, defaultArgument));

        if (!_lexer.consumeType(TokenType::COMMA)) {
            // If there isn't a comma then we expect there to be a closing parenthesis
            break;
        }
    }

    if (!_lexer.consumeType(TokenType::TEMPLATEEND)) {
        printError("expected end '>' for template data declaration! (found '" + _lexer.peekToken().currentSymbol + "')",
                   _lexer.peekToken().startPosition, _lexer.peekToken().endPosition);
        return std::vector<TemplateParameterDecl*>();
    }

    _lexer.setRightShiftState(oldRightShiftEnabledValue);

    return result;
}

/**
 * Generic function to parse the parameter list of a function. This can be used for both normal functions and member functions
 *
 * @param startPosition
 * @return `std::vector` of parsed `ParameterDecl`s
 */
std::vector<ParameterDecl*> Parser::parseParameterDecls(TextPosition startPosition) {
    std::vector<ParameterDecl*> result{};

    TextPosition endPosition = _lexer.peekToken().endPosition;

    if (!_lexer.consumeType(TokenType::LPAREN)) {
        printError("expected start '(' for function declaration! (found '" + _lexer.peekToken().currentSymbol + "')", startPosition, endPosition);
        return std::vector<ParameterDecl*>();
    }

    for (Token peekedToken = _lexer.peekToken();
         peekedToken.tokenType != TokenType::RPAREN && peekedToken.tokenType != TokenType::ENDOFFILE;
         peekedToken = _lexer.peekToken()) {
        Type* paramType = parseType();

        peekedToken = _lexer.peekToken();

        std::string paramName = peekedToken.currentSymbol;
        endPosition = _lexer.peekToken().endPosition;

        if (!_lexer.consumeType(TokenType::SYMBOL)) {
            printError("expected parameter name! (found '" + _lexer.peekToken().currentSymbol + "')", peekedToken.startPosition, peekedToken.endPosition);
            return std::vector<ParameterDecl*>();
        }

        peekedToken = _lexer.peekToken();

        Expr* defaultArgument = nullptr;

        if (peekedToken.tokenType == TokenType::EQUALS) {
            _lexer.consumeType(TokenType::EQUALS);
            defaultArgument = parseExpr(false, false);
            endPosition = defaultArgument->endPosition();
        }

        // Add new 'ParameterDecl' to the result list
        result.push_back(new ParameterDecl(paramName, _filePath, paramType->startPosition(), endPosition,
                                           paramType, defaultArgument));

        if (!_lexer.consumeType(TokenType::COMMA)) {
            // If there isn't a comma then we expect there to be a closing parenthesis
            break;
        }
    }

    if (!_lexer.consumeType(TokenType::RPAREN)) {
        printError("expected end ')' for function declaration! (found '" + _lexer.peekToken().currentSymbol + "')",
                   _lexer.peekToken().startPosition, _lexer.peekToken().endPosition);
        return std::vector<ParameterDecl*>();
    }

    return result;
}

/**
 * Parse a type declaration. Handles `int const`, `const int`, `box<T>`, `const int*const`, `int const *const *`, etc.
 *
 * @return - can be any child of the `Type` class
 */
Type* Parser::parseType(bool parseSuffix) {
    Token peekedToken = _lexer.peekToken();
    Type* result = nullptr;

    switch (peekedToken.tokenType) {
        case TokenType::CONST: {
            _lexer.consumeType(TokenType::CONST);

            if (_lexer.peekType() == TokenType::LPAREN) {
                // Type constructor?...
                _lexer.consumeType(TokenType::LPAREN);
                result = parseType();
                if (!_lexer.consumeType(TokenType::RPAREN)) {
                    printError("expected closing ')' to `const` type constructor!",
                               _lexer.peekToken().startPosition, _lexer.peekToken().endPosition);
                }
            } else {
                result = parseType(false);
            }

            if (result->qualifier() == TypeQualifier::Const) {
                printWarning("duplicate `const` qualifier not needed!",
                             result->startPosition(), result->endPosition());
            } else if (result->qualifier() == TypeQualifier::Mut) {
                printError("using `mut` and `const` at the same time is not allowed!",
                           result->startPosition(), result->endPosition());
            }

            result->setQualifier(TypeQualifier::Const);
            break;
        }
        case TokenType::MUT: {
            _lexer.consumeType(TokenType::MUT);

            if (_lexer.peekType() == TokenType::LPAREN) {
                // Type constructor?...
                _lexer.consumeType(TokenType::LPAREN);
                result = parseType();
                if (!_lexer.consumeType(TokenType::RPAREN)) {
                    printError("expected closing ')' to `const` type constructor!",
                               _lexer.peekToken().startPosition, _lexer.peekToken().endPosition);
                }
            } else {
                result = parseType(false);
            }

            if (result->qualifier() == TypeQualifier::Const) {
                printError("using `mut` and `const` at the same time is not allowed!",
                           result->startPosition(), result->endPosition());
            } else if (result->qualifier() == TypeQualifier::Mut) {
                printWarning("duplicate `mut` qualifier not needed!",
                             result->startPosition(), result->endPosition());
            }

            result->setQualifier(TypeQualifier::Mut);
            break;
        }
        case TokenType::SYMBOL: {
            std::vector<std::string> namespacePath{};
            std::string typeName = peekedToken.currentSymbol;
            _lexer.consumeType(TokenType::SYMBOL);

            while (_lexer.peekType() == TokenType::PERIOD) {
                _lexer.consumeType(TokenType::PERIOD);
                namespacePath.push_back(typeName);

                typeName = _lexer.peekToken().currentSymbol;

                if (!_lexer.consumeType(TokenType::SYMBOL)) {
                    printError("expected namespace or type identifier after `.`, found '" + typeName + "'!",
                               _lexer.peekToken().startPosition, _lexer.peekToken().endPosition);
                    return nullptr;
                }
            }

            std::vector<Expr*> templateArguments{};

            if (_lexer.peekType() == TokenType::LESS) {
                _lexer.consumeType(TokenType::LESS);

                // We tell the lexer to NOT combine two `>` operators into a single `>>` token.
                // this also tells the lexer to return `TEMPLATEEND` instead of `GREATER` for `>`
                // We back up the old state so we can return to it later. Allowing nested states for whether this is on or off
                bool oldRightShiftEnabledValue = _lexer.getRightShiftState();
                _lexer.setRightShiftState(false);

                // Parse until we find the closing `>` or until we hit the end of the file
                while (_lexer.peekType() != TokenType::TEMPLATEEND && _lexer.peekType() != TokenType::ENDOFFILE) {
                    templateArguments.push_back(parseExpr(true, true));

                    // If consuming a comma failed then break, this is a quick an easy operation.
                    if (!_lexer.consumeType(TokenType::COMMA)) break;
                }

                if (!_lexer.consumeType(TokenType::TEMPLATEEND)) {
                    printError("expected closing '>' for template type reference! (found: '" + _lexer.peekToken().currentSymbol + "')",
                               _lexer.peekToken().startPosition, _lexer.peekToken().endPosition);
                }

                // Return to the old state for whether the lexer should combine two `>` into a `>>` Token
                _lexer.setRightShiftState(oldRightShiftEnabledValue);
            }

            result = new UnresolvedType(peekedToken.startPosition, peekedToken. endPosition, TypeQualifier::None,
                                        std::move(namespacePath), typeName, templateArguments);
            break;
        }
        default:
            printError("unexpected token where type was expected! (found '" + _lexer.peekToken().currentSymbol + "')",
                       peekedToken.startPosition, peekedToken.endPosition);
            return nullptr;
    }

    if (parseSuffix) {
        return parseTypeSuffix(result);
    }

    return result;
}

Type* Parser::parseTypeSuffix(Type* type) {
    Token peekedToken = _lexer.peekToken();
    Type* result = type;

    for (peekedToken = _lexer.peekToken();
         peekedToken.tokenType != TokenType::ENDOFFILE &&
         (peekedToken.tokenType == TokenType::CONST ||
          peekedToken.tokenType == TokenType::MUT ||
          peekedToken.tokenType == TokenType::STAR ||
          peekedToken.tokenType == TokenType::AMPERSAND ||
          peekedToken.tokenType == TokenType::CARET ||
          peekedToken.tokenType == TokenType::PERCENT ||
          peekedToken.tokenType == TokenType::QUESTION);
         peekedToken = _lexer.peekToken()) {
        switch (peekedToken.tokenType) {
            case TokenType::CONST:
                _lexer.consumeType(TokenType::CONST);

                if (result->qualifier() == TypeQualifier::Const) {
                    printWarning("duplicate `const` qualifier not needed!",
                                 result->startPosition(), result->endPosition());
                } else if (result->qualifier() == TypeQualifier::Mut) {
                    printError("using `mut` and `const` at the same time is not allowed!",
                               result->startPosition(), result->endPosition());
                }

                result->setQualifier(TypeQualifier::Const);
                break;
            case TokenType::MUT:
                _lexer.consumeType(TokenType::MUT);

                if (result->qualifier() == TypeQualifier::Const) {
                    printError("using `mut` and `const` at the same time is not allowed!",
                               result->startPosition(), result->endPosition());
                } else if (result->qualifier() == TypeQualifier::Mut) {
                    printWarning("duplicate `mut` qualifier not needed!",
                                 result->startPosition(), result->endPosition());
                }

                result->setQualifier(TypeQualifier::Mut);
                break;
            case TokenType::STAR:
                _lexer.consumeType(TokenType::STAR);
                result = new PointerType(peekedToken.startPosition, peekedToken.endPosition, TypeQualifier::None,
                                         result);
                break;
            case TokenType::AMPERSAND:
                _lexer.consumeType(TokenType::AMPERSAND);
                result = new ReferenceType(peekedToken.startPosition, peekedToken.endPosition, TypeQualifier::None,
                                           result);
                break;
            default:
                printError("custom type suffixes not yet supported!", peekedToken.startPosition,
                           peekedToken.endPosition);
                return nullptr;
        }
    }

    return result;
}

/**
 * Parses a single line statement based on the lexer input
 *
 * @return any child of the class `Stmt` or `Expr` (a child of `Stmt`)
 */
Stmt *Parser::parseStmt() {
    // Remove unneeded semicolons...
    while (_lexer.peekType() == TokenType::SEMICOLON) {
        _lexer.consumeType(TokenType::SEMICOLON);
    }

    Token peekedToken = _lexer.peekToken();
    TextPosition startPosition = peekedToken.startPosition;
    TextPosition endPosition;

    switch (peekedToken.tokenType) {
        case TokenType::LCURLY:
            return parseCompoundStmt();
        case TokenType::RETURN:
            return parseReturnStmt();
        case TokenType::IF:
            return parseIfStmt();
        case TokenType::WHILE:
            return parseWhileStmt();
        case TokenType::FOR:
            return parseForStmt();
        case TokenType::DO:
            return parseDoStmt();
        case TokenType::SWITCH:
            return parseSwitchStmt();
        case TokenType::BREAK:
            return parseBreakStmt();
        case TokenType::CONTINUE:
            return parseContinueStmt();
        case TokenType::GOTO:
            return parseGotoStmt();
        case TokenType::ASM:
            // TODO: Support `asm`
            // I would like to support something better than the GCC syntax for this
            // What would be amazing would be to allow inline assembly for this but that is probably a far off
            // goal that will only happen once I implement my own assemblers.
            printError("'asm' statements not yet supported!",
                       _lexer.peekToken().startPosition, _lexer.peekToken().endPosition);
            return nullptr;
        case TokenType::TRY:
            return parseTryStmt();
        default: {
            Expr* result = parseExpr(true, true);

            // If the next token is a ':' and the result is basically a symbol then we assume this is a label
            if (_lexer.peekType() == TokenType::COLON && llvm::isa<IdentifierExpr>(result)) {
                auto* identifier = llvm::dyn_cast<IdentifierExpr>(result);

                // If it has template arguments we let the semicolon consumer throw the error, else we transform to a label
                if (!identifier->hasTemplateArguments()) {
                    _lexer.consumeType(TokenType::COLON);

                    // We can assume if the next token is '}' then there is no Stmt to label.
                    if (_lexer.peekType() == TokenType::RCURLY) {
                        printError("expected statement after label! (found '" + _lexer.peekToken().currentSymbol + "')",
                                   _lexer.peekToken().startPosition, _lexer.peekToken().endPosition);
                        return nullptr;
                    }

                    Stmt* labeledStmt = parseStmt();
                    endPosition = labeledStmt->endPosition();

                    std::string label = identifier->name();

                    delete identifier;

                    return new LabeledStmt(startPosition, endPosition, label, labeledStmt);
                }
            }

            // TODO: At some point we should support expressions with attached blocks. e.g. `expr { compound-stmt }`
            // the default support for this should be for classes `object(args) { set properties }`
            // and arrays `int[] { 1, 2, 3, 4 }`
            // but we should also maybe allow custom expressions (or constexpr?) to support having their own blocks
            if (!_lexer.consumeType(TokenType::SEMICOLON)) {
                printError("expected ';' after expression! (found '" + _lexer.peekToken().currentSymbol + "')",
                           _lexer.peekToken().startPosition, _lexer.peekToken().endPosition);
            }

            return result;
        }
    }
}

/**
 * Parse multiple lines of `Stmt` within two `{` and `}` tokens
 *
 * @return parsed `CompoundStmt`
 */
CompoundStmt *Parser::parseCompoundStmt() {
    std::vector<Stmt*> result{};

    Token peekedToken = _lexer.peekToken();
    TextPosition startPosition = peekedToken.startPosition;
    TextPosition endPosition = peekedToken.endPosition;

    if (!_lexer.consumeType(TokenType::LCURLY)) {
        printError("expected starting '{'! (found '" + _lexer.peekToken().currentSymbol + "')", startPosition, endPosition);
        return nullptr;
    }

    for (peekedToken = _lexer.peekToken();
         peekedToken.tokenType != TokenType::RCURLY && peekedToken.tokenType != TokenType::ENDOFFILE;
         peekedToken = _lexer.peekToken()) {
        result.push_back(parseStmt());
    }

    // Set the end position to the end position of the ending curly bracket.
    endPosition = _lexer.peekToken().endPosition;

    if (!_lexer.consumeType(TokenType::RCURLY)) {
        printError("expected ending '}'! (found '" + _lexer.peekToken().currentSymbol + "')", startPosition, endPosition);
        return nullptr;
    }

    return new CompoundStmt(startPosition, endPosition, std::move(result));
}

/**
 * Parse the return statement, this can either have a return value or not
 *
 * @return parsed `ReturnStmt`
 */
ReturnStmt *Parser::parseReturnStmt() {
    TextPosition startPosition = _lexer.peekToken().startPosition;
    TextPosition endPosition;

    if (!_lexer.consumeType(TokenType::RETURN)) {
        printError("expected 'return' statement! (found '" + _lexer.peekToken().currentSymbol + "')",
                   _lexer.peekToken().startPosition, _lexer.peekToken().endPosition);
        return nullptr;
    }

    Expr* returnValue = nullptr;

    if (_lexer.peekType() != TokenType::SEMICOLON) {
        returnValue = parseExpr(false, false);
    }

    endPosition = _lexer.peekToken().endPosition;

    if (!_lexer.consumeType(TokenType::SEMICOLON)) {
        printError("expected ';' to end return statement! (found '" + _lexer.peekToken().currentSymbol + "')",
                   _lexer.peekToken().startPosition, _lexer.peekToken().endPosition);
        return nullptr;
    }

    return new ReturnStmt(startPosition, endPosition, returnValue);
}

/**
 * Parse an if statement and its else statement if it exists
 * If statements are NOT required to have `CompoundStmt`, just `Stmt` can work
 *
 * @return parsed `IfStmt`
 */
IfStmt *Parser::parseIfStmt() {
    TextPosition startPosition = _lexer.peekToken().startPosition;
    TextPosition endPosition;

    if (!_lexer.consumeType(TokenType::IF)) {
        printError("expected 'if' statement! (found '" + _lexer.peekToken().currentSymbol + "')",
                   _lexer.peekToken().startPosition, _lexer.peekToken().endPosition);
        return nullptr;
    }

    if (!_lexer.consumeType(TokenType::LPAREN)) {
        printError("expected '(' after 'if' statement! (found '" + _lexer.peekToken().currentSymbol + "')",
                   _lexer.peekToken().startPosition, _lexer.peekToken().endPosition);
        return nullptr;
    }

    Expr* condition = parseExpr(false, false);

    if (!_lexer.consumeType(TokenType::RPAREN)) {
        printError("expected closing ')' after 'if' statement condition! (found '" + _lexer.peekToken().currentSymbol + "')",
                   _lexer.peekToken().startPosition, _lexer.peekToken().endPosition);
        return nullptr;
    }

    Stmt* trueStmt = nullptr;

    if (!_lexer.consumeType(TokenType::SEMICOLON)) {
        trueStmt = parseStmt();
    }

    Stmt* falseStmt = nullptr;

    if (_lexer.consumeType(TokenType::ELSE)) {
        falseStmt = parseStmt();
        endPosition = falseStmt->endPosition();
    } else {
        endPosition = trueStmt->endPosition();
    }

    return new IfStmt(startPosition, endPosition, condition, trueStmt, falseStmt);
}

WhileStmt *Parser::parseWhileStmt() {
    TextPosition startPosition = _lexer.peekToken().startPosition;
    TextPosition endPosition;

    if (!_lexer.consumeType(TokenType::WHILE)) {
        printError("expected 'while' statement! (found '" + _lexer.peekToken().currentSymbol + "')",
                   _lexer.peekToken().startPosition, _lexer.peekToken().endPosition);
        return nullptr;
    }

    if (!_lexer.consumeType(TokenType::LPAREN)) {
        printError("expected '(' after 'while' statement! (found '" + _lexer.peekToken().currentSymbol + "')",
                   _lexer.peekToken().startPosition, _lexer.peekToken().endPosition);
        return nullptr;
    }

    Expr* condition = parseExpr(false, false);

    if (!_lexer.consumeType(TokenType::RPAREN)) {
        printError("expected closing ')' after 'while' statement condition! (found '" + _lexer.peekToken().currentSymbol + "')",
                   _lexer.peekToken().startPosition, _lexer.peekToken().endPosition);
        return nullptr;
    }

    Stmt* loopStmt = nullptr;

    if (!_lexer.consumeType(TokenType::SEMICOLON)) {
        loopStmt = parseStmt();
        endPosition = loopStmt->endPosition();
    }

    return new WhileStmt(startPosition, endPosition, condition, loopStmt);
}

ForStmt *Parser::parseForStmt() {
    TextPosition startPosition = _lexer.peekToken().startPosition;
    TextPosition endPosition;

    if (!_lexer.consumeType(TokenType::FOR)) {
        printError("expected 'for' statement! (found '" + _lexer.peekToken().currentSymbol + "')",
                   _lexer.peekToken().startPosition, _lexer.peekToken().endPosition);
        return nullptr;
    }

    if (!_lexer.consumeType(TokenType::LPAREN)) {
        printError("expected '(' after 'for' statement! (found '" + _lexer.peekToken().currentSymbol + "')",
                   _lexer.peekToken().startPosition, _lexer.peekToken().endPosition);
        return nullptr;
    }

    // These are optional, if 'condition' is null then the loop is infinite
    Expr* preLoop = nullptr;
    Expr* condition = nullptr;
    Expr* iterationExpr = nullptr;

    // Check for preloop condition
    if (_lexer.peekType() != TokenType::SEMICOLON) {
        // Double check that the for loop is correctly formatted (i.e. not `for ()` it should at least be `for (;;)`
        if (_lexer.peekType() == TokenType::RPAREN) {
            printError("expected a preloop expression or ';' in 'for' statement! (found '" + _lexer.peekToken().currentSymbol + "')",
                       _lexer.peekToken().startPosition, _lexer.peekToken().endPosition);
            return nullptr;
        }

        preLoop = parseExpr(true, true);
    }

    // Try to consume the semicolon
    if (!_lexer.consumeType(TokenType::SEMICOLON)) {
        if (_lexer.peekType() == TokenType::RPAREN) {
            printError("expected a ';' after preloop expression in 'for' statement! (found '" + _lexer.peekToken().currentSymbol + "')",
                       _lexer.peekToken().startPosition, _lexer.peekToken().endPosition);
            return nullptr;
        }
    }

    // Check for condition
    if (_lexer.peekType() != TokenType::SEMICOLON) {
        // Double check that the for loop is correctly formatted (i.e. not `for (;)` it should at least be `for (;;)`
        if (_lexer.peekType() == TokenType::RPAREN) {
            printError("expected a loop condition or ';' in 'for' statement! (found '" + _lexer.peekToken().currentSymbol + "')",
                       _lexer.peekToken().startPosition, _lexer.peekToken().endPosition);
            return nullptr;
        }

        condition = parseExpr(false, false);
    }

    // Try to consume the semicolon
    if (!_lexer.consumeType(TokenType::SEMICOLON)) {
        if (_lexer.peekType() == TokenType::RPAREN) {
            printError("expected a ';' after condition in 'for' statement! (found '" + _lexer.peekToken().currentSymbol + "')",
                       _lexer.peekToken().startPosition, _lexer.peekToken().endPosition);
            return nullptr;
        }
    }

    if (_lexer.peekType() != TokenType::RPAREN) {
        // TODO: Support comma
        iterationExpr = parseExpr(false, false);
    }

    if (!_lexer.consumeType(TokenType::RPAREN)) {
        if (_lexer.peekType() == TokenType::RPAREN) {
            printError("expected closing ')' after 'for' statement! (found '" + _lexer.peekToken().currentSymbol + "')",
                       _lexer.peekToken().startPosition, _lexer.peekToken().endPosition);
            return nullptr;
        }
    }

    Stmt* loopStmt = nullptr;

    if (!_lexer.consumeType(TokenType::SEMICOLON)) {
        loopStmt = parseStmt();
        endPosition = loopStmt->endPosition();
    }

    return new ForStmt(startPosition, endPosition, preLoop, condition, iterationExpr, loopStmt);
}

DoStmt *Parser::parseDoStmt() {
    TextPosition startPosition = _lexer.peekToken().startPosition;
    TextPosition endPosition;

    if (!_lexer.consumeType(TokenType::DO)) {
        printError("expected 'do' statement! (found '" + _lexer.peekToken().currentSymbol + "')",
                   _lexer.peekToken().startPosition, _lexer.peekToken().endPosition);
        return nullptr;
    }

    // We don't require 'CompoundStmt', we allow `do callFunction(); while (true);`
    Stmt* loopStmt = nullptr;

    if (!_lexer.consumeType(TokenType::SEMICOLON)) {
        loopStmt = parseStmt();
    }

    if (!_lexer.consumeType(TokenType::WHILE)) {
        printError("expected matching 'while' statement for 'do' statement! (found '" + _lexer.peekToken().currentSymbol + "')",
                   _lexer.peekToken().startPosition, _lexer.peekToken().endPosition);
        return nullptr;
    }

    // Parse condition
    if (!_lexer.consumeType(TokenType::LPAREN)) {
        printError("expected '(' after 'while' statement! (found '" + _lexer.peekToken().currentSymbol + "')",
                   _lexer.peekToken().startPosition, _lexer.peekToken().endPosition);
        return nullptr;
    }

    Expr* condition = parseExpr(false, false);

    if (!_lexer.consumeType(TokenType::RPAREN)) {
        printError("expected closing ')' after 'while' statement condition! (found '" + _lexer.peekToken().currentSymbol + "')",
                   _lexer.peekToken().startPosition, _lexer.peekToken().endPosition);
        return nullptr;
    }

    endPosition = _lexer.peekToken().endPosition;

    if (!_lexer.consumeType(TokenType::SEMICOLON)) {
        printError("expected ';' after 'while' statement condition! (found '" + _lexer.peekToken().currentSymbol + "')",
                   _lexer.peekToken().startPosition, _lexer.peekToken().endPosition);
        return nullptr;
    }

    return new DoStmt(startPosition, endPosition, condition, loopStmt);
}

SwitchStmt *Parser::parseSwitchStmt() {
    TextPosition startPosition = _lexer.peekToken().startPosition;
    TextPosition endPosition;

    if (!_lexer.consumeType(TokenType::SWITCH)) {
        printError("expected 'switch' statement! (found '" + _lexer.peekToken().currentSymbol + "')",
                   _lexer.peekToken().startPosition, _lexer.peekToken().endPosition);
        return nullptr;
    }

    // Parse condition
    if (!_lexer.consumeType(TokenType::LPAREN)) {
        printError("expected '(' after 'while' statement! (found '" + _lexer.peekToken().currentSymbol + "')",
                   _lexer.peekToken().startPosition, _lexer.peekToken().endPosition);
        return nullptr;
    }

    Expr* condition = parseExpr(false, false);

    if (!_lexer.consumeType(TokenType::RPAREN)) {
        printError("expected closing ')' after 'while' statement condition! (found '" + _lexer.peekToken().currentSymbol + "')",
                   _lexer.peekToken().startPosition, _lexer.peekToken().endPosition);
        return nullptr;
    }

    // Parse cases
    if (!_lexer.consumeType(TokenType::LCURLY)) {
        printError("expected opening '{' after 'switch' statement condition! (found '" + _lexer.peekToken().currentSymbol + "')",
                   _lexer.peekToken().startPosition, _lexer.peekToken().endPosition);
        return nullptr;
    }

    std::vector<CaseStmt*> cases{};

    while (_lexer.peekType() != TokenType::RCURLY && _lexer.peekType() != TokenType::ENDOFFILE) {
        bool isDefault = false;
        Expr* caseCondition = nullptr;
        Stmt* trueStmt = nullptr;
        TextPosition caseStartPosition = _lexer.peekToken().startPosition;
        TextPosition caseEndPosition;

        if (_lexer.peekType() == TokenType::CASE) {
            _lexer.consumeType(TokenType::CASE);

            caseCondition = parseExpr(true, true);

            caseEndPosition = _lexer.peekToken().endPosition;
            if (!_lexer.consumeType(TokenType::COLON)) {
                printError("expected ':' after 'case' statement condition! (found '" + _lexer.peekToken().currentSymbol + "')",
                           _lexer.peekToken().startPosition, _lexer.peekToken().endPosition);
                return nullptr;
            }

            if (_lexer.peekType() != TokenType::CASE && _lexer.peekType() != TokenType::DEFAULT) {
                trueStmt = parseStmt();
                caseEndPosition = trueStmt->endPosition();
            }
        } else if (_lexer.peekType() == TokenType::DEFAULT) {
            _lexer.consumeType(TokenType::DEFAULT);
            isDefault = true;

            caseEndPosition = _lexer.peekToken().endPosition;
            if (!_lexer.consumeType(TokenType::COLON)) {
                printError("expected ':' after switch 'default' statement! (found '" + _lexer.peekToken().currentSymbol + "')",
                           _lexer.peekToken().startPosition, _lexer.peekToken().endPosition);
                return nullptr;
            }

            if (_lexer.peekType() != TokenType::CASE && _lexer.peekType() != TokenType::DEFAULT) {
                trueStmt = parseStmt();
                caseEndPosition = trueStmt->endPosition();
            }
        } else {
            trueStmt = parseStmt();
            caseEndPosition = trueStmt->endPosition();
        }

        cases.push_back(new CaseStmt(caseStartPosition, caseEndPosition, caseCondition, trueStmt, isDefault));
    }

    endPosition = _lexer.peekToken().endPosition;
    if (!_lexer.consumeType(TokenType::RCURLY)) {
        printError("expected ending '}' after 'switch' statement condition! (found '" + _lexer.peekToken().currentSymbol + "')",
                   _lexer.peekToken().startPosition, _lexer.peekToken().endPosition);
        return nullptr;
    }

    return new SwitchStmt(startPosition, endPosition, condition, cases);
}

/**
 * Parse a break statement with an option label to break from
 * With this we can label specific statements and `break` to the end of them.
 *
 * @return parsed `BreakStmt`
 */
BreakStmt *Parser::parseBreakStmt() {
    TextPosition startPosition = _lexer.peekToken().startPosition;
    TextPosition endPosition;

    if (!_lexer.consumeType(TokenType::BREAK)) {
        printError("expected 'break' statement! (found '" + _lexer.peekToken().currentSymbol + "')",
                   _lexer.peekToken().startPosition, _lexer.peekToken().endPosition);
        return nullptr;
    }

    std::string labelName;

    if (_lexer.peekType() != TokenType::SEMICOLON) {
        if (_lexer.peekType() != TokenType::SYMBOL) {
            printError("expected label name after 'break' statement! (found '" + _lexer.peekToken().currentSymbol + "')",
                       _lexer.peekToken().startPosition, _lexer.peekToken().endPosition);
            return nullptr;
        }

        labelName = _lexer.peekToken().currentSymbol;

        _lexer.consumeType(TokenType::SYMBOL);
    }

    endPosition = _lexer.peekToken().endPosition;
    if (!_lexer.consumeType(TokenType::SEMICOLON)) {
        printError("expected ';' after 'break' statement! (found '" + _lexer.peekToken().currentSymbol + "')",
                   _lexer.peekToken().startPosition, _lexer.peekToken().endPosition);
        return nullptr;
    }

    return new BreakStmt(startPosition, endPosition, labelName);
}

/**
 * Parse a continue statement with an option label to continue
 * With this we can label specific statements and `continue` it
 *
 * @return parsed `ContinueStmt`
 */
ContinueStmt *Parser::parseContinueStmt() {
    TextPosition startPosition = _lexer.peekToken().startPosition;
    TextPosition endPosition;

    if (!_lexer.consumeType(TokenType::CONTINUE)) {
        printError("expected 'continue' statement! (found '" + _lexer.peekToken().currentSymbol + "')",
                   _lexer.peekToken().startPosition, _lexer.peekToken().endPosition);
        return nullptr;
    }

    std::string labelName;

    if (_lexer.peekType() != TokenType::SEMICOLON) {
        if (_lexer.peekType() != TokenType::SYMBOL) {
            printError("expected label name after 'continue' statement! (found '" + _lexer.peekToken().currentSymbol + "')",
                       _lexer.peekToken().startPosition, _lexer.peekToken().endPosition);
            return nullptr;
        }

        labelName = _lexer.peekToken().currentSymbol;

        _lexer.consumeType(TokenType::SYMBOL);
    }

    endPosition = _lexer.peekToken().endPosition;
    if (!_lexer.consumeType(TokenType::SEMICOLON)) {
        printError("expected ';' after 'continue' statement! (found '" + _lexer.peekToken().currentSymbol + "')",
                   _lexer.peekToken().startPosition, _lexer.peekToken().endPosition);
        return nullptr;
    }

    return new ContinueStmt(startPosition, endPosition, labelName);
}

/**
 * Parse a goto statement with a label to `goto`
 * With this we can label specific statements and `goto` it
 *
 * @return parsed `GotoStmt`
 */
GotoStmt *Parser::parseGotoStmt() {
    TextPosition startPosition = _lexer.peekToken().startPosition;
    TextPosition endPosition;

    if (!_lexer.consumeType(TokenType::GOTO)) {
        printError("expected 'goto' statement! (found '" + _lexer.peekToken().currentSymbol + "')",
                   _lexer.peekToken().startPosition, _lexer.peekToken().endPosition);
        return nullptr;
    }

    if (_lexer.peekType() != TokenType::SYMBOL) {
        printError("expected label name after 'goto' statement! (found '" + _lexer.peekToken().currentSymbol + "')",
                   _lexer.peekToken().startPosition, _lexer.peekToken().endPosition);
        return nullptr;
    }

    std::string labelName = _lexer.peekToken().currentSymbol;

    _lexer.consumeType(TokenType::SYMBOL);

    endPosition = _lexer.peekToken().endPosition;
    if (!_lexer.consumeType(TokenType::SEMICOLON)) {
        printError("expected ';' after 'goto' statement! (found '" + _lexer.peekToken().currentSymbol + "')",
                   _lexer.peekToken().startPosition, _lexer.peekToken().endPosition);
        return nullptr;
    }

    return new GotoStmt(startPosition, endPosition, labelName);
}

TryStmt *Parser::parseTryStmt() {
    TextPosition startPosition = _lexer.peekToken().startPosition;
    TextPosition endPosition;

    if (!_lexer.consumeType(TokenType::TRY)) {
        printError("expected 'try' statement! (found '" + _lexer.peekToken().currentSymbol + "')",
                   _lexer.peekToken().startPosition, _lexer.peekToken().endPosition);
        return nullptr;
    }

    CompoundStmt* encapsulatedStmt = parseCompoundStmt();
    std::vector<TryCatchStmt*> catchStmts{};
    TryFinallyStmt* finallyStmt = nullptr;

    while (_lexer.peekType() == TokenType::CATCH || _lexer.peekType() == TokenType::FINALLY) {
        if (_lexer.peekType() == TokenType::CATCH) {
            TextPosition catchStartPosition = _lexer.peekToken().startPosition;
            TextPosition catchEndPosition;

            _lexer.consumeType(TokenType::CATCH);

            Type* catchType = nullptr;
            std::string exceptionVarName;

            // If there is a '(' then we will parse the exception variable, if not that is okay. There doesn't have to be one.
            if (_lexer.consumeType(TokenType::LPAREN)) {
                catchType = parseType();

                if (_lexer.peekType() == TokenType::SYMBOL) {
                    exceptionVarName = _lexer.peekToken().currentSymbol;

                    _lexer.consumeType(TokenType::SYMBOL);
                }

                if (!_lexer.consumeType(TokenType::RPAREN)) {
                    printError("expected closing ')' for catch statement! (found '" + _lexer.peekToken().currentSymbol + "')",
                               _lexer.peekToken().startPosition, _lexer.peekToken().endPosition);
                    return nullptr;
                }
            }

            CompoundStmt* compoundStmt = parseCompoundStmt();
            catchEndPosition = compoundStmt->endPosition();

            catchStmts.push_back(new TryCatchStmt(catchStartPosition, catchEndPosition, catchType, exceptionVarName, compoundStmt));

            endPosition = catchEndPosition;
        } else if (_lexer.peekType() == TokenType::FINALLY) {
            if (finallyStmt != nullptr) {
                printError("duplicate 'finally' statement for 'try' statement!",
                           _lexer.peekToken().startPosition, _lexer.peekToken().endPosition);
                return nullptr;
            }

            TextPosition finallyStartPosition = _lexer.peekToken().startPosition;
            TextPosition finallyEndPosition;

            _lexer.consumeType(TokenType::FINALLY);

            CompoundStmt* compoundStmt = parseCompoundStmt();
            finallyEndPosition = compoundStmt->endPosition();

            finallyStmt = new TryFinallyStmt(finallyStartPosition, finallyEndPosition, compoundStmt);

            endPosition = finallyEndPosition;
        }
    }

    return new TryStmt(startPosition, endPosition, encapsulatedStmt, catchStmts, finallyStmt);
}

/**
 * Parse a generic expression
 *
 * @param isStatement - tells the parser that the expression is a statement and can be a local variable declaration
 * @param templateTypingAllowed - tells the parser that template typing is allowed
 * @return generic parsed `Expr`
 */
Expr *Parser::parseExpr(bool isStatement, bool templateTypingAllowed) {
    return parseAssignmentMisc(isStatement, templateTypingAllowed);
}

/**
 * First call `parseLogicalOr` then check for any assignment operators.
 * If an assignment operator is found it then recursively calls back to itsself to parse the right hand side of the assignment
 * This implicitly converts:
 *     i = j = k = 12;
 * To:
 *     i = (j = (k = 12));
 *
 * @param isStatement - tells the parser that the expression is a statement and can be a local variable declaration
 * @param templateTypingAllowed - tells the parser that template typing is allowed
 * @return generic `BinaryOperatorExpr` or assignment `BinaryOperatorExpr`
 */
Expr *Parser::parseAssignmentMisc(bool isStatement, bool templateTypingAllowed) {
    TextPosition startPosition = _lexer.peekToken().startPosition;
    TextPosition endPosition;
    Expr* result = parseLogicalOr(isStatement, templateTypingAllowed);

    // TODO: We need to handle more than just `SYMBOL`
    if (isStatement && _lexer.peekType() == TokenType::SYMBOL) {
        // Variable names cannot have generics so we ignore generics (TODO: But type names can have generics? we shouldn't stop them from being parsed...)
        // TODO: NOTE: We WILL parse postfix `++` and postfix `--` here. If this is a custom prefix operator call then
        //  postfix `++`/`--` should be performed on the result of the operator...
        Expr* identifier = parseCallPostfixOrMemberAccess(false, true);
        endPosition = identifier->endPosition();
        result = new LocalVariableDeclOrPrefixOperatorCallExpr(startPosition, endPosition, result, identifier);
    }

    switch (_lexer.peekType()) {
        case TokenType::QUESTION: {
            _lexer.consumeType(TokenType::QUESTION);
            Expr* trueExpr = parseAssignmentMisc(false, false);

            if (!_lexer.consumeType(TokenType::COLON)) {
                printError("expected ':' in ternary statement! (found '" + _lexer.peekToken().currentSymbol + "')",
                           _lexer.peekToken().startPosition, _lexer.peekToken().endPosition);
            }

            Expr* falseExpr = parseAssignmentMisc(false, false);
            endPosition = falseExpr->endPosition();

            return new TernaryExpr(startPosition, endPosition, result, trueExpr, falseExpr);
        }
        case TokenType::EQUALS: {
            _lexer.consumeType(TokenType::EQUALS);
            Expr* rvalue = parseAssignmentMisc(false, false);
            endPosition = rvalue->endPosition();

            return new BinaryOperatorExpr(startPosition, endPosition, "=", result, rvalue);
        }
        case TokenType::PLUSEQUALS: {
            _lexer.consumeType(TokenType::PLUSEQUALS);
            Expr* rvalue = parseAssignmentMisc(false, false);
            endPosition = rvalue->endPosition();

            return new BinaryOperatorExpr(startPosition, endPosition, "+=", result, rvalue);
        }
        case TokenType::MINUSEQUALS: {
            _lexer.consumeType(TokenType::MINUSEQUALS);
            Expr* rvalue = parseAssignmentMisc(false, false);
            endPosition = rvalue->endPosition();

            return new BinaryOperatorExpr(startPosition, endPosition, "-=", result, rvalue);
        }
        case TokenType::STAREQUALS: {
            _lexer.consumeType(TokenType::STAREQUALS);
            Expr* rvalue = parseAssignmentMisc(false, false);
            endPosition = rvalue->endPosition();

            return new BinaryOperatorExpr(startPosition, endPosition, "*=", result, rvalue);
        }
        case TokenType::SLASHEQUALS: {
            _lexer.consumeType(TokenType::SLASHEQUALS);
            Expr* rvalue = parseAssignmentMisc(false, false);
            endPosition = rvalue->endPosition();

            return new BinaryOperatorExpr(startPosition, endPosition, "/=", result, rvalue);
        }
        case TokenType::PERCENTEQUALS: {
            _lexer.consumeType(TokenType::PERCENTEQUALS);
            Expr* rvalue = parseAssignmentMisc(false, false);
            endPosition = rvalue->endPosition();

            return new BinaryOperatorExpr(startPosition, endPosition, "%=", result, rvalue);
        }
        case TokenType::LEFTEQUALS: {
            _lexer.consumeType(TokenType::LEFTEQUALS);
            Expr* rvalue = parseAssignmentMisc(false, false);
            endPosition = rvalue->endPosition();

            return new BinaryOperatorExpr(startPosition, endPosition, "<<=", result, rvalue);
        }
        case TokenType::RIGHTEQUALS: {
            _lexer.consumeType(TokenType::RIGHTEQUALS);
            Expr* rvalue = parseAssignmentMisc(false, false);
            endPosition = rvalue->endPosition();

            return new BinaryOperatorExpr(startPosition, endPosition, ">>=", result, rvalue);
        }
        case TokenType::AMPERSANDEQUALS: {
            _lexer.consumeType(TokenType::AMPERSANDEQUALS);
            Expr* rvalue = parseAssignmentMisc(false, false);
            endPosition = rvalue->endPosition();

            return new BinaryOperatorExpr(startPosition, endPosition, "&=", result, rvalue);
        }
        case TokenType::CARETEQUALS: {
            _lexer.consumeType(TokenType::CARETEQUALS);
            Expr* rvalue = parseAssignmentMisc(false, false);
            endPosition = rvalue->endPosition();

            return new BinaryOperatorExpr(startPosition, endPosition, "^=", result, rvalue);
        }
        case TokenType::PIPEEQUALS: {
            _lexer.consumeType(TokenType::PIPEEQUALS);
            Expr* rvalue = parseAssignmentMisc(false, false);
            endPosition = rvalue->endPosition();

            return new BinaryOperatorExpr(startPosition, endPosition, "|=", result, rvalue);
        }
        default:
            return result;
    }
}

/**
 * First call `parseLogicalAnd` then check for the logical or operator `||`.
 * If a logical or operator is found it then loops to keep checking for more `||` operators
 * This implicitly converts:
 *     i || j || k
 * To:
 *     (i || j) || k;
 *
 * @param templateTypingAllowed - tells the parser that template typing is allowed
 * @return generic `BinaryOperatorExpr` or logical or `BinaryOperatorExpr`
 */
Expr *Parser::parseLogicalOr(bool isStatement, bool templateTypingAllowed) {
    TextPosition startPosition = _lexer.peekToken().startPosition;
    TextPosition endPosition;
    Expr* result = parseLogicalAnd(isStatement, templateTypingAllowed);

    while (_lexer.peekType() != TokenType::ENDOFFILE) {
        switch (_lexer.peekType()) {
            case TokenType::PIPEPIPE: {
                _lexer.consumeType(TokenType::PIPEPIPE);
                Expr* rvalue = parseLogicalAnd(false, false);
                endPosition = rvalue->endPosition();

                result = new BinaryOperatorExpr(startPosition, endPosition, "||", result, rvalue);
                continue;
            }
            default:
                return result;
        }
    }

    printError("reach end of file unexpectedly!", _lexer.peekToken().startPosition, _lexer.peekToken().endPosition);
    return nullptr;
}

/**
 * First call `parseBitwiseOr` then check for the logical and operator `&&`.
 * If a logical and operator is found it then loops to keep checking for more `&&` operators
 * This implicitly converts:
 *     i && j && k
 * To:
 *     (i && j) && k;
 *
 * @param templateTypingAllowed - tells the parser that template typing is allowed
 * @return generic `BinaryOperatorExpr` or logical and `BinaryOperatorExpr`
 */
Expr *Parser::parseLogicalAnd(bool isStatement, bool templateTypingAllowed) {
    TextPosition startPosition = _lexer.peekToken().startPosition;
    TextPosition endPosition;
    Expr* result = parseBitwiseOr(isStatement, templateTypingAllowed);

    while (_lexer.peekType() != TokenType::ENDOFFILE) {
        switch (_lexer.peekType()) {
            case TokenType::AMPERSANDAMPERSAND: {
                _lexer.consumeType(TokenType::AMPERSANDAMPERSAND);
                Expr* rvalue = parseBitwiseOr(false, false);
                endPosition = rvalue->endPosition();

                result = new BinaryOperatorExpr(startPosition, endPosition, "&&", result, rvalue);
                continue;
            }
            default:
                return result;
        }
    }

    printError("reach end of file unexpectedly!", _lexer.peekToken().startPosition, _lexer.peekToken().endPosition);
    return nullptr;
}

/**
 * First call `parseBitwiseXor` then check for the bitwise or operator `|`.
 * If a bitwise or operator is found it then loops to keep checking for more `|` operators
 * This implicitly converts:
 *     i | j | k
 * To:
 *     (i | j) | k;
 *
 * @param templateTypingAllowed - tells the parser that template typing is allowed
 * @return generic `BinaryOperatorExpr` or bitwise or `BinaryOperatorExpr`
 */
Expr *Parser::parseBitwiseOr(bool isStatement, bool templateTypingAllowed) {
    TextPosition startPosition = _lexer.peekToken().startPosition;
    TextPosition endPosition;
    Expr* result = parseBitwiseXor(isStatement, templateTypingAllowed);

    while (_lexer.peekType() != TokenType::ENDOFFILE) {
        switch (_lexer.peekType()) {
            case TokenType::PIPE: {
                _lexer.consumeType(TokenType::PIPE);
                Expr* rvalue = parseBitwiseXor(false, false);
                endPosition = rvalue->endPosition();

                result = new BinaryOperatorExpr(startPosition, endPosition, "|", result, rvalue);
                continue;
            }
            default:
                return result;
        }
    }

    printError("reach end of file unexpectedly!", _lexer.peekToken().startPosition, _lexer.peekToken().endPosition);
    return nullptr;
}

/**
 * First call `parseBitwiseAnd` then check for the bitwise xor operator `^`.
 * If a bitwise xor operator is found it then loops to keep checking for more `^` operators
 * This implicitly converts:
 *     i ^ j ^ k
 * To:
 *     (i ^ j) ^ k;
 *
 * @param templateTypingAllowed - tells the parser that template typing is allowed
 * @return generic `BinaryOperatorExpr` or bitwise xor `BinaryOperatorExpr`
 */
Expr *Parser::parseBitwiseXor(bool isStatement, bool templateTypingAllowed) {
    TextPosition startPosition = _lexer.peekToken().startPosition;
    TextPosition endPosition;
    Expr* result = parseBitwiseAnd(isStatement, templateTypingAllowed);

    while (_lexer.peekType() != TokenType::ENDOFFILE) {
        switch (_lexer.peekType()) {
            case TokenType::CARET: {
                _lexer.consumeType(TokenType::CARET);
                Expr* rvalue = parseBitwiseAnd(false, false);
                endPosition = rvalue->endPosition();

                result = new BinaryOperatorExpr(startPosition, endPosition, "^", result, rvalue);
                continue;
            }
            default:
                return result;
        }
    }

    printError("reach end of file unexpectedly!", _lexer.peekToken().startPosition, _lexer.peekToken().endPosition);
    return nullptr;
}

/**
 * First call `parseEqualToNotEqualTo` then check for the bitwise and operator `&`.
 * If a bitwise and operator is found it then loops to keep checking for more `&` operators
 * This implicitly converts:
 *     i & j & k
 * To:
 *     (i & j) & k;
 *
 * @param templateTypingAllowed - tells the parser that template typing is allowed
 * @return generic `BinaryOperatorExpr` or bitwise and `BinaryOperatorExpr`
 */
Expr *Parser::parseBitwiseAnd(bool isStatement, bool templateTypingAllowed) {
    TextPosition startPosition = _lexer.peekToken().startPosition;
    TextPosition endPosition;
    Expr* result = parseEqualToNotEqualTo(isStatement, templateTypingAllowed);

    while (_lexer.peekType() != TokenType::ENDOFFILE) {
        switch (_lexer.peekType()) {
            case TokenType::AMPERSAND: {
                _lexer.consumeType(TokenType::AMPERSAND);
                Expr* rvalue = parseEqualToNotEqualTo(false, false);
                endPosition = rvalue->endPosition();

                result = new BinaryOperatorExpr(startPosition, endPosition, "&", result, rvalue);
                continue;
            }
            default:
                return result;
        }
    }

    printError("reach end of file unexpectedly!", _lexer.peekToken().startPosition, _lexer.peekToken().endPosition);
    return nullptr;
}

/**
 * First call `parseGreaterThanLessThan` then check for `==` and `!=`.
 * If either are found it then loops to keep checking for more `==` or `!=` operators
 * This implicitly converts:
 *     i == j != k
 * To:
 *     (i == j) != k;
 *
 * @param templateTypingAllowed - tells the parser that template typing is allowed
 * @return generic `BinaryOperatorExpr` or an equal or not equal to `BinaryOperatorExpr`
 */
Expr *Parser::parseEqualToNotEqualTo(bool isStatement, bool templateTypingAllowed) {
    TextPosition startPosition = _lexer.peekToken().startPosition;
    TextPosition endPosition;
    Expr* result = parseGreaterThanLessThan(isStatement, templateTypingAllowed);

    while (_lexer.peekType() != TokenType::ENDOFFILE) {
        switch (_lexer.peekType()) {
            case TokenType::EQUALEQUALS: {
                _lexer.consumeType(TokenType::EQUALEQUALS);
                Expr* rvalue = parseGreaterThanLessThan(false, false);
                endPosition = rvalue->endPosition();

                result = new BinaryOperatorExpr(startPosition, endPosition, "==", result, rvalue);
                continue;
            }
            case TokenType::NOTEQUALS: {
                _lexer.consumeType(TokenType::NOTEQUALS);
                Expr* rvalue = parseGreaterThanLessThan(false, false);
                endPosition = rvalue->endPosition();

                result = new BinaryOperatorExpr(startPosition, endPosition, "!=", result, rvalue);
                continue;
            }
            default:
                return result;
        }
    }

    printError("reach end of file unexpectedly!", _lexer.peekToken().startPosition, _lexer.peekToken().endPosition);
    return nullptr;
}

/**
 * First call `parseBitwiseShifts` then check for `>`, `<`, `>=`, and `<=`.
 * If either are found it then loops to keep checking for more `>`, `<`, `>=`, or `<=` operators
 * This implicitly converts:
 *     i < j > k
 * To:
 *     (i < j) > k;
 *
 * @param templateTypingAllowed - tells the parser that template typing is allowed
 * @return generic `BinaryOperatorExpr` or a greater or less than `BinaryOperatorExpr`
 */
Expr *Parser::parseGreaterThanLessThan(bool isStatement, bool templateTypingAllowed) {
    TextPosition startPosition = _lexer.peekToken().startPosition;
    TextPosition endPosition;
    Expr* result = parseBitwiseShifts(isStatement, templateTypingAllowed);

    while (_lexer.peekType() != TokenType::ENDOFFILE) {
        switch (_lexer.peekType()) {
            case TokenType::GREATER: {
                _lexer.consumeType(TokenType::GREATER);
                Expr* rvalue = parseBitwiseShifts(false, false);
                endPosition = rvalue->endPosition();

                result = new BinaryOperatorExpr(startPosition, endPosition, ">", result, rvalue);
                continue;
            }
            case TokenType::GREATEREQUALS: {
                _lexer.consumeType(TokenType::GREATEREQUALS);
                Expr* rvalue = parseBitwiseShifts(false, false);
                endPosition = rvalue->endPosition();

                result = new BinaryOperatorExpr(startPosition, endPosition, ">=", result, rvalue);
                continue;
            }
            case TokenType::LESS: {
                _lexer.consumeType(TokenType::LESS);
                Expr* rvalue = parseBitwiseShifts(false, false);
                endPosition = rvalue->endPosition();

                result = new BinaryOperatorExpr(startPosition, endPosition, "<", result, rvalue);
                continue;
            }
            case TokenType::LESSEQUALS: {
                _lexer.consumeType(TokenType::LESSEQUALS);
                Expr* rvalue = parseBitwiseShifts(false, false);
                endPosition = rvalue->endPosition();

                result = new BinaryOperatorExpr(startPosition, endPosition, "<=", result, rvalue);
                continue;
            }
            default:
                return result;
        }
    }

    printError("reach end of file unexpectedly!", _lexer.peekToken().startPosition, _lexer.peekToken().endPosition);
    return nullptr;
}

/**
 * First call `parseAdditionSubtraction` then check for `>>` and `<<`.
 * If either are found it then loops to keep checking for more `>>` or `<<` operators
 * This implicitly converts:
 *     i << j >> k
 * To:
 *     (i << j) >> k;
 *
 * @param templateTypingAllowed - tells the parser that template typing is allowed
 * @return generic `BinaryOperatorExpr` or a bit shift left or right `BinaryOperatorExpr`
 */
Expr *Parser::parseBitwiseShifts(bool isStatement, bool templateTypingAllowed) {
    TextPosition startPosition = _lexer.peekToken().startPosition;
    TextPosition endPosition;
    Expr* result = parseAdditionSubtraction(isStatement, templateTypingAllowed);

    while (_lexer.peekType() != TokenType::ENDOFFILE) {
        switch (_lexer.peekType()) {
            case TokenType::LEFT: {
                _lexer.consumeType(TokenType::LEFT);
                Expr* rvalue = parseAdditionSubtraction(false, false);
                endPosition = rvalue->endPosition();

                result = new BinaryOperatorExpr(startPosition, endPosition, "<<", result, rvalue);
                continue;
            }
            case TokenType::RIGHT: {
                _lexer.consumeType(TokenType::RIGHT);
                Expr* rvalue = parseAdditionSubtraction(false, false);
                endPosition = rvalue->endPosition();

                result = new BinaryOperatorExpr(startPosition, endPosition, ">>", result, rvalue);
                continue;
            }
            default:
                return result;
        }
    }

    printError("reach end of file unexpectedly!", _lexer.peekToken().startPosition, _lexer.peekToken().endPosition);
    return nullptr;
}

/**
 * First call `parseMultiplicationDivisionOrRemainder` then check for `+` and `-`.
 * If either are found it then loops to keep checking for more `+` or `-` operators
 * This implicitly converts:
 *     i + j - k
 * To:
 *     (i + j) - k;
 *
 * @param templateTypingAllowed - tells the parser that template typing is allowed
 * @return generic `BinaryOperatorExpr` or a addition or subtraction `BinaryOperatorExpr`
 */
Expr *Parser::parseAdditionSubtraction(bool isStatement, bool templateTypingAllowed) {
    TextPosition startPosition = _lexer.peekToken().startPosition;
    TextPosition endPosition;
    Expr* result = parseMultiplicationDivisionOrRemainder(isStatement, templateTypingAllowed);

    while (_lexer.peekType() != TokenType::ENDOFFILE) {
        switch (_lexer.peekType()) {
            case TokenType::PLUS: {
                _lexer.consumeType(TokenType::PLUS);
                Expr* rvalue = parseMultiplicationDivisionOrRemainder(false, false);
                endPosition = rvalue->endPosition();

                result = new BinaryOperatorExpr(startPosition, endPosition, "+", result, rvalue);
                continue;
            }
            case TokenType::MINUS: {
                _lexer.consumeType(TokenType::MINUS);
                Expr* rvalue = parseMultiplicationDivisionOrRemainder(false, false);
                endPosition = rvalue->endPosition();

                result = new BinaryOperatorExpr(startPosition, endPosition, "-", result, rvalue);
                continue;
            }
            default:
                return result;
        }
    }

    printError("reach end of file unexpectedly!", _lexer.peekToken().startPosition, _lexer.peekToken().endPosition);
    return nullptr;
}

/**
 * First call `parsePrefixes` then check for `*`, `/` and `%`. // TODO: Should we support the VB `\` integer division?
 * If either are found it then loops to keep checking for more `*`, `/`, or `%` operators
 * This implicitly converts:
 *     i * j % k
 * To:
 *     (i * j) % k;
 *
 * @param templateTypingAllowed - tells the parser that template typing is allowed
 * @return generic `BinaryOperatorExpr` or a addition or subtraction `BinaryOperatorExpr`
 */
Expr *Parser::parseMultiplicationDivisionOrRemainder(bool isStatement, bool templateTypingAllowed) {
    TextPosition startPosition = _lexer.peekToken().startPosition;
    TextPosition endPosition;
    Expr* result = parsePrefixes(isStatement, templateTypingAllowed);

    while (_lexer.peekType() != TokenType::ENDOFFILE) {
        switch (_lexer.peekType()) {
            case TokenType::STAR: {
                _lexer.consumeType(TokenType::STAR);
                Expr* rvalue = parsePrefixes(false, false);
                endPosition = rvalue->endPosition();

                result = new BinaryOperatorExpr(startPosition, endPosition, "*", result, rvalue);
                continue;
            }
            case TokenType::SLASH: {
                _lexer.consumeType(TokenType::SLASH);
                Expr* rvalue = parsePrefixes(false, false);
                endPosition = rvalue->endPosition();

                result = new BinaryOperatorExpr(startPosition, endPosition, "/", result, rvalue);
                continue;
            }
            case TokenType::PERCENT: {
                _lexer.consumeType(TokenType::PERCENT);
                Expr* rvalue = parsePrefixes(false, false);
                endPosition = rvalue->endPosition();

                result = new BinaryOperatorExpr(startPosition, endPosition, "%", result, rvalue);
                continue;
            }
            default:
                return result;
        }
    }

    printError("reach end of file unexpectedly!", _lexer.peekToken().startPosition, _lexer.peekToken().endPosition);
    return nullptr;
}

/**
 * First we check for any prefix operators:
 * (`++`, `--`, `+`, `-`, `!`, `~`, `*`, `&`, `sizeof`, `alignof`, offsetof`, `nameof`, and `(` for `ParenExpr`)
 * It then calls back on itself until there is no other prefix operator detected
 * If no prefix operator is detected we then call `parseCallPostfixOrMemberAccess`
 *
 * @param templateTypingAllowed
 * @return
 */
Expr *Parser::parsePrefixes(bool isStatement, bool templateTypingAllowed) {
    TextPosition startPosition = _lexer.peekToken().startPosition;
    TextPosition endPosition;

    switch (_lexer.peekType()) {
        case TokenType::PLUSPLUS: {
            _lexer.consumeType(TokenType::PLUSPLUS);
            Expr* expr = parsePrefixes(false, false);
            endPosition = expr->endPosition();
            return new PrefixOperatorExpr(startPosition, endPosition, "++", expr);
        }
        case TokenType::MINUSMINUS: {
            _lexer.consumeType(TokenType::MINUSMINUS);
            Expr* expr = parsePrefixes(false, false);
            endPosition = expr->endPosition();
            return new PrefixOperatorExpr(startPosition, endPosition, "--", expr);
        }
        case TokenType::PLUS: {
            _lexer.consumeType(TokenType::PLUS);
            Expr* expr = parsePrefixes(false, false);
            endPosition = expr->endPosition();
            return new PrefixOperatorExpr(startPosition, endPosition, "+", expr);
        }
        case TokenType::MINUS: {
            _lexer.consumeType(TokenType::MINUS);
            Expr* expr = parsePrefixes(false, false);
            endPosition = expr->endPosition();
            return new PrefixOperatorExpr(startPosition, endPosition, "-", expr);
        }
        case TokenType::NOT: {
            _lexer.consumeType(TokenType::NOT);
            Expr* expr = parsePrefixes(false, false);
            endPosition = expr->endPosition();
            return new PrefixOperatorExpr(startPosition, endPosition, "!", expr);
        }
        case TokenType::TILDE: {
            _lexer.consumeType(TokenType::TILDE);
            Expr* expr = parsePrefixes(false, false);
            endPosition = expr->endPosition();
            return new PrefixOperatorExpr(startPosition, endPosition, "~", expr);
        }
        case TokenType::STAR: {
            _lexer.consumeType(TokenType::STAR);
            Expr* expr = parsePrefixes(false, false);
            endPosition = expr->endPosition();
            return new PrefixOperatorExpr(startPosition, endPosition, "*", expr);
        }
        case TokenType::AMPERSAND: {
            _lexer.consumeType(TokenType::AMPERSAND);
            Expr* expr = parsePrefixes(false, false);
            endPosition = expr->endPosition();
            return new PrefixOperatorExpr(startPosition, endPosition, "&", expr);
        }
        case TokenType::SIZEOF: {
            _lexer.consumeType(TokenType::SIZEOF);
            Expr* expr = parsePrefixes(false, false);
            endPosition = expr->endPosition();
            return new PrefixOperatorExpr(startPosition, endPosition, "sizeof", expr);
        }
        case TokenType::ALIGNOF: {
            _lexer.consumeType(TokenType::ALIGNOF);
            Expr* expr = parsePrefixes(false, false);
            endPosition = expr->endPosition();
            return new PrefixOperatorExpr(startPosition, endPosition, "alignof", expr);
        }
        case TokenType::OFFSETOF: {
            _lexer.consumeType(TokenType::OFFSETOF);
            Expr* expr = parsePrefixes(false, false);
            endPosition = expr->endPosition();
            return new PrefixOperatorExpr(startPosition, endPosition, "offsetof", expr);
        }
        case TokenType::NAMEOF: {
            _lexer.consumeType(TokenType::NAMEOF);
            Expr* expr = parsePrefixes(false, false);
            endPosition = expr->endPosition();
            return new PrefixOperatorExpr(startPosition, endPosition, "nameof", expr);
        }
        case TokenType::LPAREN: {
            bool oldRightShiftEnabledValue = _lexer.getRightShiftState();
            _lexer.setRightShiftState(true);
            _lexer.consumeType(TokenType::LPAREN);

            Expr* nestedExpr = parseExpr(true, false);

            endPosition = _lexer.peekToken().endPosition;

            if (!_lexer.consumeType(TokenType::RPAREN)) {
                printError("expected ending ')'! (found '" + _lexer.peekToken().currentSymbol + "')", startPosition, endPosition);
                return nullptr;
            }

            _lexer.setRightShiftState(oldRightShiftEnabledValue);

            // Kind of verbose but this should give us the ability to do casting...
            if (_lexer.peekType() == TokenType::SYMBOL ||
                _lexer.peekType() == TokenType::LPAREN ||
                _lexer.peekType() == TokenType::PLUSPLUS ||
                _lexer.peekType() == TokenType::MINUSMINUS ||
                _lexer.peekType() == TokenType::NOT ||
                _lexer.peekType() == TokenType::TILDE ||
                _lexer.peekType() == TokenType::SIZEOF ||
                _lexer.peekType() == TokenType::ALIGNOF ||
                _lexer.peekType() == TokenType::OFFSETOF ||
                _lexer.peekType() == TokenType::NAMEOF ||
                _lexer.peekType() == TokenType::NUMBER) {
                Expr* castee = parsePrefixes(false, false);
                endPosition = castee->endPosition();
                return new PotentialExplicitCastExpr(startPosition, endPosition, nestedExpr, castee);
            } else {
                return new ParenExpr(startPosition, endPosition, nestedExpr);
            }
        }
        // TODO: Support this too
//        case TokenType.SYMBOL:
//            RST.Expr parsedSymbol1 = ParseCallSuffixOrMemberAccess(templateTypingAllowed);
//
//            // TODO: Should this also support 'RST.SingleIdentifier'?
//            if (parsedSymbol1 is RST.Identifier && lexer.PeekType() == TokenType.SYMBOL)
//            {
//                RST.Expr parsedSymbol2 = ParseCallSuffixOrMemberAccess(false);
//
//                if (parsedSymbol2 is RST.Identifier && lexer.PeekType() == TokenType.SYMBOL)
//                {
//                    RST.Expr parsedSymbol3 = ParseCallSuffixOrMemberAccess(false);
//
//                    if (parsedSymbol3 is RST.Identifier && lexer.PeekType() == TokenType.SYMBOL)
//                    {
//                        // TODO: MultiIdentifier
//                        List<RST.Expr> expressions = new List<RST.Expr>()
//                        {
//                            parsedSymbol1,
//                                    parsedSymbol2,
//                                    parsedSymbol3
//                        };
//
//                        // This should support `prefix1 prefix2 prefix3 functionCall()` and `prefix variableCall infix variableCall2`
//                        // and `prefix arr[0] infix functionCall()`
//                        // BUT it will also catch `prefix variableCall infix functionCall() postfix` which I'm not sure we want to support?
//                        while (lexer.PeekType() == TokenType.SYMBOL)
//                        {
//                            expressions.Add(ParseCallSuffixOrMemberAccess(false));
//                        }
//
//                        endPosition = expressions[expressions.Count - 1].EndPosition;
//
//                        return new RST.MultipleIdentifierCallExpr(startPosition, endPosition, expressions);
//                    }
//                    else
//                    {
//                        endPosition = parsedSymbol3.EndPosition;
//
//                        return new RST.CustomBinaryOperatorExpr(startPosition, endPosition, parsedSymbol1, parsedSymbol2 as RST.Identifier, parsedSymbol3);
//                    }
//                }
//                else
//                {
//                    endPosition = parsedSymbol2.EndPosition;
//
//                    return new RST.IdentifierPrefixExpr(startPosition, endPosition, parsedSymbol1 as RST.Identifier, parsedSymbol2);
//                }
//            }
//            else
//            {
//                return parsedSymbol1;
//            }
        default:
            return parseCallPostfixOrMemberAccess(isStatement, templateTypingAllowed);
    }
}

/**
 * First we call `parseVariableLiteralOrParen` then we check for any postfix operator
 * (`++`, `--`, `(` for function calls`, `[` for array indexing, `.` for member access, or `->` for dereference then member access)
 * Then we loop to keep looking for more postfix operators, if none are found we return the result
 *
 * @param templateTypingAllowed
 * @return
 */
Expr *Parser::parseCallPostfixOrMemberAccess(bool isStatement, bool templateTypingAllowed) {
    TextPosition startPosition = _lexer.peekToken().startPosition;
    TextPosition endPosition;
    Expr* result = parseVariableLiteralOrParen(isStatement, templateTypingAllowed);

    while (_lexer.peekType() != TokenType::ENDOFFILE) {
        switch (_lexer.peekType()) {
            case TokenType::PLUSPLUS:
                endPosition = _lexer.peekToken().endPosition;
                _lexer.consumeType(TokenType::PLUSPLUS);
                result = new PostfixOperatorExpr(startPosition, endPosition, "++", result);
                continue;
            case TokenType::MINUSMINUS:
                endPosition = _lexer.peekToken().endPosition;
                _lexer.consumeType(TokenType::MINUSMINUS);
                result = new PostfixOperatorExpr(startPosition, endPosition, "--", result);
                continue;
            case TokenType::LPAREN: {
                _lexer.consumeType(TokenType::LPAREN);

                std::vector<Expr*> arguments{};

                while (_lexer.peekType() != TokenType::RPAREN && _lexer.peekType() != TokenType::ENDOFFILE) {
                    arguments.push_back(parseExpr(false, false));

                    if (!_lexer.consumeType(TokenType::COMMA)) break;
                }

                endPosition = _lexer.peekToken().endPosition;

                if (!_lexer.consumeType(TokenType::RPAREN)) {
                    printError("expected ending ')' for function call! (found '" + _lexer.peekToken().currentSymbol + "')",
                               _lexer.peekToken().startPosition, _lexer.peekToken().endPosition);
                    return nullptr;
                }

                result = new FunctionCallExpr(startPosition, endPosition, result, arguments);
                continue;
            }
            case TokenType::LSQUARE: {
                _lexer.consumeType(TokenType::LSQUARE);

                std::vector<Expr*> arguments{};

                while (_lexer.peekType() != TokenType::RSQUARE && _lexer.peekType() != TokenType::ENDOFFILE) {
                    arguments.push_back(parseExpr(false, false));

                    if (!_lexer.consumeType(TokenType::COMMA)) break;
                }

                endPosition = _lexer.peekToken().endPosition;

                if (!_lexer.consumeType(TokenType::RSQUARE)) {
                    printError("expected ending ']' for indexer call! (found '" + _lexer.peekToken().currentSymbol + "')",
                               _lexer.peekToken().startPosition, _lexer.peekToken().endPosition);
                    return nullptr;
                }

                result = new IndexerCallExpr(startPosition, endPosition, result, arguments);
                continue;
            }
            case TokenType::PERIOD: {
                _lexer.consumeType(TokenType::PERIOD);

                IdentifierExpr* member = parseIdentifier(false);
                endPosition = member->endPosition();
                result = new MemberAccessCallExpr(startPosition, endPosition, false, result, member);
                continue;
            }
            case TokenType::ARROW: {
                _lexer.consumeType(TokenType::ARROW);

                IdentifierExpr* member = parseIdentifier(false);
                endPosition = member->endPosition();
                result = new MemberAccessCallExpr(startPosition, endPosition, true, result, member);
                continue;
            }
            default:
                return result;
        }
    }

    printError("reach end of file unexpectedly!", _lexer.peekToken().startPosition, _lexer.peekToken().endPosition);
    return nullptr;
}

/**
 * Parses `IdentifierExpr`s (`varName`, `functionName<T>`), number literals (`12`, `1.234`, `0b010`, `0xff`, `07`), string literals (`"string literal"`), and character literals (`'C'`)
 * It then immediately returns the result
 * If nothing is found we then return null
 *
 * @param templateTypingAllowed
 * @return
 */
Expr *Parser::parseVariableLiteralOrParen(bool isStatement, bool templateTypingAllowed) {
    Token peekedToken = _lexer.peekToken();
    TextPosition startPosition = peekedToken.startPosition;
    TextPosition endPosition = peekedToken.endPosition;

    switch (peekedToken.tokenType) {
        case TokenType::SYMBOL: {
            IdentifierExpr* result = parseIdentifier(templateTypingAllowed, false);

            // TODO: Support `&`, `^`, `%`, `$`, `?'
            // For this point what we do is: if there is a `*` after the symbol we check the token after the `*` to
            //  see if it is another `*`, `)` (for casting), `>` (for templates)`, `mut`, `const`, etc.
            // If there is one of the above Tokens then we parse the type suffix and the check for a symbol after the
            //  type for local variable declarations
            if (isStatement && _lexer.peekType() == TokenType::STAR) {
                LexerCheckpoint checkpoint = _lexer.createCheckpoint();

                _lexer.consumeType(TokenType::STAR);

                if (_lexer.peekType() == TokenType::STAR ||
                    _lexer.peekType() == TokenType::RPAREN ||
                    _lexer.peekType() == TokenType::TEMPLATEEND ||
                    _lexer.peekType() == TokenType::MUT ||
                    _lexer.peekType() == TokenType::CONST) {
                    auto unresolvedType = new UnresolvedType(result->startPosition(), result->endPosition(), TypeQualifier::None,
                                                             {}, result->name(), std::move(result->templateArguments));
                    delete result;

                    auto unresolvedPointerType = new PointerType(unresolvedType->startPosition(),
                                                                 unresolvedType->endPosition(), TypeQualifier::None,
                                                                 unresolvedType);

                    Expr* potentialResult =  new UnresolvedTypeRefExpr(unresolvedPointerType->startPosition(),
                                                                       unresolvedPointerType->endPosition(),
                                                                       parseTypeSuffix(unresolvedPointerType));

                    // If there is a symbol after then we immediately know it is a variable declaration
                    if (_lexer.peekType() == TokenType::SYMBOL) {
                        std::string varName = _lexer.peekToken().currentSymbol;
                        endPosition = _lexer.peekToken().endPosition;

                        _lexer.consumeType(TokenType::SYMBOL);

                        return new LocalVariableDeclExpr(startPosition, endPosition, potentialResult, varName);
                    } else {
                        return potentialResult;
                    }
                } else {
                    _lexer.returnToCheckpoint(checkpoint);
                }
            }

            if (_lexer.peekType() == TokenType::MUT ||
                _lexer.peekType() == TokenType::CONST) {
                auto unresolvedType = new UnresolvedType(result->startPosition(), result->endPosition(), TypeQualifier::None,
                                                         {}, result->name(), std::move(result->templateArguments));

                delete result;

                Expr* potentialResult =  new UnresolvedTypeRefExpr(unresolvedType->startPosition(),
                                                                   unresolvedType->endPosition(),
                                                                   parseTypeSuffix(unresolvedType));

                // If there is a symbol after then we immediately know it is a variable declaration
                if (_lexer.peekType() == TokenType::SYMBOL) {
                    std::string varName = _lexer.peekToken().currentSymbol;
                    endPosition = _lexer.peekToken().endPosition;

                    _lexer.consumeType(TokenType::SYMBOL);

                    return new LocalVariableDeclExpr(startPosition, endPosition, potentialResult, varName);
                } else {
                    return potentialResult;
                }
            }

            return result;
        }
        case TokenType::NUMBER:
            return parseNumberLiteral();
        case TokenType::STRING:
            return parseStringLiteral();
        case TokenType::CHARACTER: {
            unsigned int characterValue = peekedToken.currentChar;
            _lexer.consumeType(TokenType::CHARACTER);
            return new CharacterLiteralExpr(startPosition, endPosition, characterValue);
        }
        case TokenType::MUT:
        case TokenType::CONST: {
            // NOTE: We parse the type without parsing the type suffix (`*`, `&`, etc.) because this is adopted from C.
            // In C `const int *` is `const(int)*` or `int const*` NOT `const(int*)` or `int *const`
            Type* refType = parseType(false);
            refType = parseTypeSuffix(refType);

            Expr* potentialResult = new UnresolvedTypeRefExpr(refType->startPosition(), refType->endPosition(), refType);

            // If there is a symbol after then we immediately know it is a variable declaration
            if (_lexer.peekType() == TokenType::SYMBOL) {
                std::string varName = _lexer.peekToken().currentSymbol;
                endPosition = _lexer.peekToken().endPosition;

                _lexer.consumeType(TokenType::SYMBOL);

                return new LocalVariableDeclExpr(startPosition, endPosition, potentialResult, varName);
            } else {
                return potentialResult;
            }
        }
//        case TokenType::LPAREN: {
//            bool oldRightShiftEnabledValue = _lexer.getRightShiftState();
//            _lexer.setRightShiftState(true);
//            _lexer.consumeType(TokenType::LPAREN);
//
//            Expr* nestedExpr = parseExpr(false);
//
//            endPosition = _lexer.peekToken().endPosition;
//
//            if (!_lexer.consumeType(TokenType::RPAREN)) {
//                printError("expected ending ')'! (found '" + _lexer.peekToken().currentSymbol + "')", startPosition, endPosition);
//                return nullptr;
//            }
//
//            _lexer.setRightShiftState(oldRightShiftEnabledValue);
//
//            return new ParenExpr(startPosition, endPosition, nestedExpr);
//        }
        default:
            printError("expected constant literal or identifier! (found '" + _lexer.peekToken().currentSymbol + "')", startPosition, endPosition);
            return nullptr;
    }
}

Expr *Parser::parseNumberLiteral() {
    Token peekedToken = _lexer.peekToken();
    TextPosition startPosition = peekedToken.startPosition;
    TextPosition endPosition = peekedToken.endPosition;
    // Assume Base10
    int numberBase;
    std::string resultNumber;
    const std::string decimalValidCharacters = "0123456789";

    // <RANT>
    // Okay, how is this something that C++ didn't have until C++20??
    // How did it take until C++20 for them to decide they should add "starts_with"??
    // </RANT>
#define STARTS_WITH(check, begin) ((check).compare(0, (begin).length(), (begin)) == 0)

    // Without checking the length of the string we will confuse the literal suffixes (if a user adds one)
    bool nonBase10Supported = peekedToken.currentSymbol.length() > 2;
    // Without this we will generate a lone `0` as `IntegerLiteral(base: 8, number: "")`
    bool base8Supported = peekedToken.currentSymbol.length() > 1;

    if (nonBase10Supported && STARTS_WITH(peekedToken.currentSymbol, std::string("0x"))) {
        numberBase = 16;
        resultNumber = peekedToken.currentSymbol.substr(2);
        const std::string validCharacters = "0123456789abcdefABCDEF";

        for (const char& c : resultNumber) {
            if (validCharacters.find(c) == std::string::npos) {
                printError(std::string("hexadecimal number literal contains invalid character '") + c + "'!", startPosition, endPosition);
                return nullptr;
            }
        }
    } else if (nonBase10Supported && STARTS_WITH(peekedToken.currentSymbol, std::string("0b"))) {
        numberBase = 2;
        resultNumber = peekedToken.currentSymbol.substr(2);
        const std::string validCharacters = "01";

        for (const char& c : resultNumber) {
            if (validCharacters.find(c) == std::string::npos) {
                printError(std::string("binary number literal contains invalid character '") + c + "'!", startPosition, endPosition);
                return nullptr;
            }
        }
    } else if (base8Supported && STARTS_WITH(peekedToken.currentSymbol, std::string("0"))) {
        numberBase = 8;
        resultNumber = peekedToken.currentSymbol.substr(1);
        const std::string validCharacters = "01234567";

        for (const char& c : resultNumber) {
            if (validCharacters.find(c) == std::string::npos) {
                printError(std::string("octal number literal contains invalid character '") + c + "'!", startPosition, endPosition);
                return nullptr;
            }
        }
    } else {
        numberBase = 10;
        resultNumber = peekedToken.currentSymbol;

        for (const char& c : resultNumber) {
            if (decimalValidCharacters.find(c) == std::string::npos) {
                printError(std::string("decimal number literal contains invalid character '") + c + "'!", startPosition, endPosition);
                return nullptr;
            }
        }
    }

    if (!_lexer.consumeType(TokenType::NUMBER)) {
        printError("expected number literal! (found '" + _lexer.peekToken().currentSymbol + "')", startPosition, endPosition);
        return nullptr;
    }

    // Parse floating points...
    if (_lexer.peekType() == TokenType::PERIOD) {
        LexerCheckpoint checkpoint = _lexer.createCheckpoint();
        _lexer.consumeType(TokenType::PERIOD);

        if (_lexer.peekType() == TokenType::NUMBER) {
            if (numberBase != 10) {
                printError("float literals cannot be in any other base than base10!", startPosition, endPosition);
                return nullptr;
            }

            for (const char& c : _lexer.peekToken().currentSymbol) {
                if (decimalValidCharacters.find(c) == std::string::npos) {
                    printError(std::string("decimal number literal contains invalid character '") + c + "'!", startPosition, endPosition);
                    return nullptr;
                }
            }

            resultNumber += "." + _lexer.peekToken().currentSymbol;
            endPosition = _lexer.peekToken().endPosition;
            _lexer.consumeType(TokenType::NUMBER);

            return new FloatLiteralExpr(startPosition, endPosition, resultNumber);
        } else {
            _lexer.returnToCheckpoint(checkpoint);
        }
    }

    // If it isn't a known float we return an Integer
    return new IntegerLiteralExpr(startPosition, endPosition, numberBase, resultNumber);
}

Expr *Parser::parseStringLiteral() {
    Token peekedToken = _lexer.peekToken();
    TextPosition startPosition = peekedToken.startPosition;
    TextPosition endPosition = peekedToken.endPosition;
    std::string result = peekedToken.currentSymbol;

    // This gives us support for allowing: "This split"   " string"" to parse correctly"
    while (_lexer.peekType() == TokenType::STRING) {
        result += _lexer.peekToken().currentSymbol;
        endPosition = _lexer.peekToken().endPosition;
        _lexer.consumeType(TokenType::STRING);
    }

    return new StringLiteralExpr(startPosition, endPosition, result);
}

IdentifierExpr *Parser::parseIdentifier(bool templateTypingAllowed, bool ignoreGenerics) {
    TextPosition startPosition = _lexer.peekToken().startPosition;
    TextPosition endPosition = _lexer.peekToken().endPosition;

    std::string text = _lexer.peekToken().currentSymbol;

    if ((isdigit(text[0]) || ispunct(text[0])/* || char.IsSymbol(text[0])*/) && text[0] != '_') {
        printError("identifiers cannot start with a number or contain punctuation!", startPosition, endPosition);
        return nullptr;
    }

    for (std::size_t i = 1; i < text.length(); ++i) {
        if ((ispunct(text[i])/* || char.IsSymbol(text[i])*/) && text[i] != '_') {
            printError("identifiers cannot contain punctuation!", startPosition, endPosition);
            return nullptr;
        }
    }

    if (!_lexer.consumeType(TokenType::SYMBOL)) {
        printError("unexpected token where identifier was expected! (found '" + _lexer.peekToken().currentSymbol + "')", startPosition, endPosition);
        return nullptr;
    }

    std::vector<Expr*> templateData{};

    if (!ignoreGenerics && _lexer.peekType() == TokenType::LESS) {
        bool oldRightShiftEnabledValue = _lexer.getRightShiftState();
        _lexer.setRightShiftState(false);
        LexerCheckpoint checkpoint = _lexer.createCheckpoint();

        _lexer.consumeType(TokenType::LESS);

        while (_lexer.peekType() != TokenType::TEMPLATEEND) {
            // Is setting it to true correct?
            templateData.push_back(parsePrefixes(true, true));

            if (_lexer.peekType() != TokenType::COMMA) break;
            _lexer.consumeType(TokenType::COMMA);
        }

        bool cancelled = true;

        // We don't error out here. If the syntax doesn't match what we expect we just return to our checkpoint.
        if (_lexer.peekType() == TokenType::TEMPLATEEND) {
            TextPosition potentialNewEndPosition = _lexer.peekToken().endPosition;
            _lexer.consumeType(TokenType::TEMPLATEEND);

            if ((templateTypingAllowed && _lexer.peekType() == TokenType::SYMBOL)
                || _lexer.peekType() == TokenType::LPAREN
                || _lexer.peekType() == TokenType::RPAREN
                || _lexer.peekType() == TokenType::TEMPLATEEND
                // TODO: Is this okay?
                || _lexer.peekType() == TokenType::LCURLY
                // TODO: Should we only allow this when 'templateTypingAllowed' is true? Idk. I don't think it matters. Also we might want to support 'keyword'
                || _lexer.peekMeta() == TokenMetaType::MODIFIER) {
                cancelled = false;
                endPosition = potentialNewEndPosition;
            } else {
                _lexer.returnToCheckpoint(checkpoint);
            }
        } else {
            _lexer.returnToCheckpoint(checkpoint);
        }

        _lexer.setRightShiftState(oldRightShiftEnabledValue);

        // Delete everything in the templateData vector if parsing was cancelled
        if (cancelled && !templateData.empty()) {
            for (Expr* expr : templateData) {
                delete expr;
            }

            templateData.clear();
        }
    }

    return new IdentifierExpr(startPosition, endPosition, text, templateData);
}
