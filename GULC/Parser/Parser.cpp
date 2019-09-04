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

// TODO: Support adding the error to a list and continuing? Or just cancelling the compilation of this file?
void Parser::printError(const std::string& errorMessage, TextPosition startPosition, TextPosition endPosition) {
    std::cout << "gulc parser error[" << _filePath << ", "
                                   "{" << startPosition.line << ", " << startPosition.column << "} "
                                   "to {" << endPosition.line << ", " << endPosition.column << "}]: "
              << errorMessage
              << std::endl;

    std::exit(1);
}

// TODO: Support adding the warning to a list
void Parser::printWarning(const std::string &warningMessage, TextPosition startPosition, TextPosition endPosition) {
    std::cout << "gulc parser warning[" << _filePath << ", "
                                     "{" << startPosition.line << ", " << startPosition.column << "} "
                                     "to {" << endPosition.line << ", " << endPosition.column << "}]: "
              << warningMessage
              << std::endl;
}

Decl *Parser::parseTopLevelDecl() {
    Token peekedToken = _lexer.peekToken();
    TextPosition startPosition = peekedToken.startPosition;
    TextPosition endPosition = peekedToken.endPosition;

    bool isConst = false;
    bool isExtern = false;
    bool isVolatile = false;
    bool isAbstract = false;
    bool isSealed = false;
    /*
		IN, // 'in'
		OUT, // 'out'
		REF, // 'ref'
     */

    while (peekedToken.metaType == TokenMetaType::MODIFIER) {
        // TODO:
        switch (peekedToken.tokenType) {
            case TokenType::PUBLIC:
            case TokenType::PRIVATE:
            case TokenType::PROTECTED:
            case TokenType::INTERNAL:
                printError("'public', 'private', 'protected', and 'internal' cannot be applied to top-level declarations!", peekedToken.startPosition, peekedToken.endPosition);
                return nullptr;
            case TokenType::STATIC:
                printError("'static' cannot be applied to top-level declarations!", peekedToken.startPosition, peekedToken.endPosition);
                return nullptr;
            case TokenType::VIRTUAL:
                printError("'virtual' cannot be applied to top-level declarations!", peekedToken.startPosition, peekedToken.endPosition);
                return nullptr;
            case TokenType::OVERRIDE:
                printError("'override' cannot be applied to top-level declarations!", peekedToken.startPosition, peekedToken.endPosition);
                return nullptr;
            case TokenType::CONST:
                if (isConst) printWarning("duplicate 'const' specifier", peekedToken.startPosition, peekedToken.endPosition);

                _lexer.consumeType(TokenType::CONST);
                isConst = true;
                break;
            case TokenType::EXTERN:
                if (isExtern) printWarning("duplicate 'extern' specifier", peekedToken.startPosition, peekedToken.endPosition);

                _lexer.consumeType(TokenType::EXTERN);
                isExtern = true;
                break;
            case TokenType::VOLATILE:
                if (isVolatile) printWarning("duplicate 'volatile' specifier", peekedToken.startPosition, peekedToken.endPosition);

                _lexer.consumeType(TokenType::VOLATILE);
                isVolatile = true;
                break;
            case TokenType::ABSTRACT:
                if (isAbstract) printWarning("duplicate 'abstract' specifier", peekedToken.startPosition, peekedToken.endPosition);

                _lexer.consumeType(TokenType::ABSTRACT);
                isAbstract = true;
                break;
            case TokenType::SEALED:
                if (isSealed) printWarning("duplicate 'sealed' specifier", peekedToken.startPosition, peekedToken.endPosition);

                _lexer.consumeType(TokenType::SEALED);
                isSealed = true;
                break;
            default:
                // TODO: Should this just be a warning instead?
                printError("unknown modifier", peekedToken.startPosition, peekedToken.endPosition);
                return nullptr;
        }

        peekedToken = _lexer.peekToken();
        endPosition = peekedToken.endPosition;
    }

    switch (peekedToken.tokenType) {
        case TokenType::NAMESPACE:
            // TODO:
            printError("namespaces not yet supported!", startPosition, endPosition);
            return nullptr;
        case TokenType::CLASS:
            // TODO:
            printError("classes not yet supported!", startPosition, endPosition);
            return nullptr;
        case TokenType::STRUCT:
            // TODO:
            printError("structs not yet supported!", startPosition, endPosition);
            return nullptr;
        case TokenType::UNION:
            // TODO:
            printError("unions not yet supported!", startPosition, endPosition);
            return nullptr;
        case TokenType::INTERFACE:
            // TODO:
            printError("interfaces not yet supported!", startPosition, endPosition);
            return nullptr;
        case TokenType::ENUM:
            // TODO:
            printError("enums not yet supported!", startPosition, endPosition);
            return nullptr;
        case TokenType::SYMBOL: {
            Type* resultType = parseType();

            peekedToken = _lexer.peekToken();
            endPosition = peekedToken.endPosition;

            std::string name;

            // Parse the rest of the type and the name of the variable or function
            switch (peekedToken.tokenType) {
                case TokenType::SYMBOL:
                    // This is the function declaration name
                    name = peekedToken.currentSymbol;
                    _lexer.consumeType(TokenType::SYMBOL);
                    break;
                default:
                    printError("unexpected token in top-level declaration! (found '" + _lexer.peekToken().currentSymbol + "')", startPosition, endPosition);
                    return nullptr;
            }

            peekedToken = _lexer.peekToken();
            endPosition = peekedToken.endPosition;

            // Detect if it is a variable or function and parse based on the detection
            switch (peekedToken.tokenType) {
                case TokenType::LESS:
                    // Template Function
                    printError("template functions not yet supported!", startPosition, endPosition);
                    return nullptr;
                case TokenType::LPAREN: { // Function
                    std::vector<ParameterDecl*> parameters = parseParameterDecls(startPosition);
                    // TODO: Allow modifiers after the end parenthesis (e.g. 'where T : IArray<?>'
                    CompoundStmt* compoundStmt = parseCompoundStmt();

                    return new FunctionDecl(_filePath, startPosition, endPosition, resultType, name, parameters, compoundStmt);
                }
                case TokenType::EQUALS:
                case TokenType::SEMICOLON:
                    // Variable
                    printError("top-level variables not yet supported!", startPosition, endPosition);
                    return nullptr;
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

        if (!_lexer.consumeType(TokenType::SYMBOL)) {
            printError("expected parameter name! (found '" + _lexer.peekToken().currentSymbol + "')", peekedToken.startPosition, peekedToken.endPosition);
            return std::vector<ParameterDecl*>();
        }

        peekedToken = _lexer.peekToken();

        if (peekedToken.tokenType == TokenType::EQUALS) {
            // Default argument
            printError("default arguments not yet supported!", peekedToken.startPosition, peekedToken.endPosition);
            return std::vector<ParameterDecl*>();
        }

        endPosition = _lexer.peekToken().endPosition;
        // Add new 'ParameterDecl' to the result list
        result.push_back(new ParameterDecl(_filePath, paramType->startPosition(), endPosition,
                                           paramType, paramName, nullptr));

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

    return std::move(result);
}

Type* Parser::parseType() {
    Token peekedToken = _lexer.peekToken();

    switch (peekedToken.tokenType) {
        case TokenType::SYMBOL: {
            std::string typeName = peekedToken.currentSymbol;
            _lexer.consumeType(TokenType::SYMBOL);
            return new UnresolvedType(peekedToken.startPosition, peekedToken. endPosition, {}, typeName);
        }
        default:
            printError("unexpected token where type was expected! (found '" + _lexer.peekToken().currentSymbol + "')",
                       peekedToken.startPosition, peekedToken.endPosition);
            return nullptr;
    }
}

Stmt *Parser::parseStmt() {
    Token peekedToken = _lexer.peekToken();
    TextPosition startPosition = peekedToken.startPosition;
    TextPosition endPosition = peekedToken.endPosition;

    switch (peekedToken.tokenType) {
        case TokenType::LCURLY:
            return parseCompoundStmt();
        case TokenType::RETURN:
            return parseReturnStmt();
        default:
            printError("unexpected token where statement was expected! (found '" + _lexer.peekToken().currentSymbol + "')", startPosition, endPosition);
            return nullptr;
    }
}

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

ReturnStmt *Parser::parseReturnStmt() {
    Token peekedToken = _lexer.peekToken();
    TextPosition startPosition = peekedToken.startPosition;
    TextPosition endPosition;

    if (!_lexer.consumeType(TokenType::RETURN)) {
        printError("expected 'return' statement! (found '" + _lexer.peekToken().currentSymbol + "')",
                   _lexer.peekToken().startPosition, _lexer.peekToken().endPosition);
        return nullptr;
    }

    Expr* returnValue = nullptr;

    if (_lexer.peekType() != TokenType::SEMICOLON) {
        returnValue = parseRValue(false);
    }

    endPosition = _lexer.peekToken().endPosition;

    if (!_lexer.consumeType(TokenType::SEMICOLON)) {
        printError("expected ';' to end return statement! (found '" + _lexer.peekToken().currentSymbol + "')",
                   _lexer.peekToken().startPosition, _lexer.peekToken().endPosition);
        return nullptr;
    }

    return new ReturnStmt(startPosition, endPosition, returnValue);
}

Expr *Parser::parseRValue(bool templateTypingAllowed) {
    return parseAssignmentMisc(templateTypingAllowed);
}

Expr *Parser::parseAssignmentMisc(bool templateTypingAllowed) {
    TextPosition startPosition = _lexer.peekToken().startPosition;
    TextPosition endPosition;
    Expr* result = parseLogicalOr(templateTypingAllowed);

    switch (_lexer.peekType()) {
        case TokenType::QUESTION: {
            _lexer.consumeType(TokenType::QUESTION);
            Expr* trueExpr = parseAssignmentMisc(false);

            if (!_lexer.consumeType(TokenType::COLON)) {
                printError("expected ':' in ternary statement! (found '" + _lexer.peekToken().currentSymbol + "')",
                           _lexer.peekToken().startPosition, _lexer.peekToken().endPosition);
            }

            Expr* falseExpr = parseAssignmentMisc(false);
            endPosition = falseExpr->endPosition();

            return new TernaryExpr(startPosition, endPosition, result, trueExpr, falseExpr);
        }
        case TokenType::EQUALS: {
            _lexer.consumeType(TokenType::EQUALS);
            Expr* rvalue = parseAssignmentMisc(false);
            endPosition = rvalue->endPosition();

            return new BinaryOperatorExpr(startPosition, endPosition, "=", result, rvalue);
        }
        case TokenType::PLUSEQUALS: {
            _lexer.consumeType(TokenType::PLUSEQUALS);
            Expr* rvalue = parseAssignmentMisc(false);
            endPosition = rvalue->endPosition();

            return new BinaryOperatorExpr(startPosition, endPosition, "+=", result, rvalue);
        }
        case TokenType::MINUSEQUALS: {
            _lexer.consumeType(TokenType::MINUSEQUALS);
            Expr* rvalue = parseAssignmentMisc(false);
            endPosition = rvalue->endPosition();

            return new BinaryOperatorExpr(startPosition, endPosition, "-=", result, rvalue);
        }
        case TokenType::STAREQUALS: {
            _lexer.consumeType(TokenType::STAREQUALS);
            Expr* rvalue = parseAssignmentMisc(false);
            endPosition = rvalue->endPosition();

            return new BinaryOperatorExpr(startPosition, endPosition, "*=", result, rvalue);
        }
        case TokenType::SLASHEQUALS: {
            _lexer.consumeType(TokenType::SLASHEQUALS);
            Expr* rvalue = parseAssignmentMisc(false);
            endPosition = rvalue->endPosition();

            return new BinaryOperatorExpr(startPosition, endPosition, "/=", result, rvalue);
        }
        case TokenType::PERCENTEQUALS: {
            _lexer.consumeType(TokenType::PERCENTEQUALS);
            Expr* rvalue = parseAssignmentMisc(false);
            endPosition = rvalue->endPosition();

            return new BinaryOperatorExpr(startPosition, endPosition, "%=", result, rvalue);
        }
        case TokenType::LEFTEQUALS: {
            _lexer.consumeType(TokenType::LEFTEQUALS);
            Expr* rvalue = parseAssignmentMisc(false);
            endPosition = rvalue->endPosition();

            return new BinaryOperatorExpr(startPosition, endPosition, "<<=", result, rvalue);
        }
        case TokenType::RIGHTEQUALS: {
            _lexer.consumeType(TokenType::RIGHTEQUALS);
            Expr* rvalue = parseAssignmentMisc(false);
            endPosition = rvalue->endPosition();

            return new BinaryOperatorExpr(startPosition, endPosition, ">>=", result, rvalue);
        }
        case TokenType::AMPERSANDEQUALS: {
            _lexer.consumeType(TokenType::AMPERSANDEQUALS);
            Expr* rvalue = parseAssignmentMisc(false);
            endPosition = rvalue->endPosition();

            return new BinaryOperatorExpr(startPosition, endPosition, "&=", result, rvalue);
        }
        case TokenType::CARETEQUALS: {
            _lexer.consumeType(TokenType::CARETEQUALS);
            Expr* rvalue = parseAssignmentMisc(false);
            endPosition = rvalue->endPosition();

            return new BinaryOperatorExpr(startPosition, endPosition, "^=", result, rvalue);
        }
        case TokenType::PIPEEQUALS: {
            _lexer.consumeType(TokenType::PIPEEQUALS);
            Expr* rvalue = parseAssignmentMisc(false);
            endPosition = rvalue->endPosition();

            return new BinaryOperatorExpr(startPosition, endPosition, "|=", result, rvalue);
        }
        default:
            return result;
    }
}

Expr *Parser::parseLogicalOr(bool templateTypingAllowed) {
    TextPosition startPosition = _lexer.peekToken().startPosition;
    TextPosition endPosition;
    Expr* result = parseLogicalAnd(templateTypingAllowed);

    while (true) {
        switch (_lexer.peekType()) {
            case TokenType::PIPEPIPE: {
                _lexer.consumeType(TokenType::PIPEPIPE);
                Expr* rvalue = parseLogicalAnd(false);
                endPosition = rvalue->endPosition();

                result = new BinaryOperatorExpr(startPosition, endPosition, "||", result, rvalue);
                continue;
            }
            default:
                return result;
        }
    }
}

Expr *Parser::parseLogicalAnd(bool templateTypingAllowed) {
    TextPosition startPosition = _lexer.peekToken().startPosition;
    TextPosition endPosition;
    Expr* result = parseBitwiseOr(templateTypingAllowed);

    while (true) {
        switch (_lexer.peekType()) {
            case TokenType::AMPERSANDAMPERSAND: {
                _lexer.consumeType(TokenType::AMPERSANDAMPERSAND);
                Expr* rvalue = parseBitwiseOr(false);
                endPosition = rvalue->endPosition();

                result = new BinaryOperatorExpr(startPosition, endPosition, "&&", result, rvalue);
                continue;
            }
            default:
                return result;
        }
    }
}

Expr *Parser::parseBitwiseOr(bool templateTypingAllowed) {
    TextPosition startPosition = _lexer.peekToken().startPosition;
    TextPosition endPosition;
    Expr* result = parseBitwiseXor(templateTypingAllowed);

    while (true) {
        switch (_lexer.peekType()) {
            case TokenType::PIPE: {
                _lexer.consumeType(TokenType::PIPE);
                Expr* rvalue = parseBitwiseXor(false);
                endPosition = rvalue->endPosition();

                result = new BinaryOperatorExpr(startPosition, endPosition, "|", result, rvalue);
                continue;
            }
            default:
                return result;
        }
    }
}

Expr *Parser::parseBitwiseXor(bool templateTypingAllowed) {
    TextPosition startPosition = _lexer.peekToken().startPosition;
    TextPosition endPosition;
    Expr* result = parseBitwiseAnd(templateTypingAllowed);

    while (true) {
        switch (_lexer.peekType()) {
            case TokenType::CARET: {
                _lexer.consumeType(TokenType::CARET);
                Expr* rvalue = parseBitwiseAnd(false);
                endPosition = rvalue->endPosition();

                result = new BinaryOperatorExpr(startPosition, endPosition, "^", result, rvalue);
                continue;
            }
            default:
                return result;
        }
    }
}

Expr *Parser::parseBitwiseAnd(bool templateTypingAllowed) {
    TextPosition startPosition = _lexer.peekToken().startPosition;
    TextPosition endPosition;
    Expr* result = parseEqualToNotEqualTo(templateTypingAllowed);

    while (true) {
        switch (_lexer.peekType()) {
            case TokenType::AMPERSAND: {
                _lexer.consumeType(TokenType::AMPERSAND);
                Expr* rvalue = parseEqualToNotEqualTo(false);
                endPosition = rvalue->endPosition();

                result = new BinaryOperatorExpr(startPosition, endPosition, "&", result, rvalue);
                continue;
            }
            default:
                return result;
        }
    }
}

Expr *Parser::parseEqualToNotEqualTo(bool templateTypingAllowed) {
    TextPosition startPosition = _lexer.peekToken().startPosition;
    TextPosition endPosition;
    Expr* result = parseGreaterThanLessThan(templateTypingAllowed);

    while (true) {
        switch (_lexer.peekType()) {
            case TokenType::EQUALEQUALS: {
                _lexer.consumeType(TokenType::EQUALEQUALS);
                Expr* rvalue = parseGreaterThanLessThan(false);
                endPosition = rvalue->endPosition();

                result = new BinaryOperatorExpr(startPosition, endPosition, "==", result, rvalue);
                continue;
            }
            case TokenType::NOTEQUALS: {
                _lexer.consumeType(TokenType::NOTEQUALS);
                Expr* rvalue = parseGreaterThanLessThan(false);
                endPosition = rvalue->endPosition();

                result = new BinaryOperatorExpr(startPosition, endPosition, "!=", result, rvalue);
                continue;
            }
            default:
                return result;
        }
    }
}

Expr *Parser::parseGreaterThanLessThan(bool templateTypingAllowed) {
    TextPosition startPosition = _lexer.peekToken().startPosition;
    TextPosition endPosition;
    Expr* result = parseBitwiseShifts(templateTypingAllowed);

    while (true) {
        switch (_lexer.peekType()) {
            case TokenType::GREATER: {
                _lexer.consumeType(TokenType::GREATER);
                Expr* rvalue = parseBitwiseShifts(false);
                endPosition = rvalue->endPosition();

                result = new BinaryOperatorExpr(startPosition, endPosition, ">", result, rvalue);
                continue;
            }
            case TokenType::GREATEREQUALS: {
                _lexer.consumeType(TokenType::GREATEREQUALS);
                Expr* rvalue = parseBitwiseShifts(false);
                endPosition = rvalue->endPosition();

                result = new BinaryOperatorExpr(startPosition, endPosition, ">=", result, rvalue);
                continue;
            }
            case TokenType::LESS: {
                _lexer.consumeType(TokenType::LESS);
                Expr* rvalue = parseBitwiseShifts(false);
                endPosition = rvalue->endPosition();

                result = new BinaryOperatorExpr(startPosition, endPosition, "<", result, rvalue);
                continue;
            }
            case TokenType::LESSEQUALS: {
                _lexer.consumeType(TokenType::LESSEQUALS);
                Expr* rvalue = parseBitwiseShifts(false);
                endPosition = rvalue->endPosition();

                result = new BinaryOperatorExpr(startPosition, endPosition, "<=", result, rvalue);
                continue;
            }
            default:
                return result;
        }
    }
}

Expr *Parser::parseBitwiseShifts(bool templateTypingAllowed) {
    TextPosition startPosition = _lexer.peekToken().startPosition;
    TextPosition endPosition;
    Expr* result = parseAdditionSubtraction(templateTypingAllowed);

    while (true) {
        switch (_lexer.peekType()) {
            case TokenType::LEFT: {
                _lexer.consumeType(TokenType::LEFT);
                Expr* rvalue = parseAdditionSubtraction(false);
                endPosition = rvalue->endPosition();

                result = new BinaryOperatorExpr(startPosition, endPosition, "<<", result, rvalue);
                continue;
            }
            case TokenType::RIGHT: {
                _lexer.consumeType(TokenType::RIGHT);
                Expr* rvalue = parseAdditionSubtraction(false);
                endPosition = rvalue->endPosition();

                result = new BinaryOperatorExpr(startPosition, endPosition, ">>", result, rvalue);
                continue;
            }
            default:
                return result;
        }
    }
}

Expr *Parser::parseAdditionSubtraction(bool templateTypingAllowed) {
    TextPosition startPosition = _lexer.peekToken().startPosition;
    TextPosition endPosition;
    Expr* result = parseMultiplicationDivisionOrRemainder(templateTypingAllowed);

    while (true) {
        switch (_lexer.peekType()) {
            case TokenType::PLUS: {
                _lexer.consumeType(TokenType::PLUS);
                Expr* rvalue = parseMultiplicationDivisionOrRemainder(false);
                endPosition = rvalue->endPosition();

                result = new BinaryOperatorExpr(startPosition, endPosition, "+", result, rvalue);
                continue;
            }
            case TokenType::MINUS: {
                _lexer.consumeType(TokenType::MINUS);
                Expr* rvalue = parseMultiplicationDivisionOrRemainder(false);
                endPosition = rvalue->endPosition();

                result = new BinaryOperatorExpr(startPosition, endPosition, "-", result, rvalue);
                continue;
            }
            default:
                return result;
        }
    }
}

Expr *Parser::parseMultiplicationDivisionOrRemainder(bool templateTypingAllowed) {
    TextPosition startPosition = _lexer.peekToken().startPosition;
    TextPosition endPosition;
    Expr* result = parsePrefixes(templateTypingAllowed);

    while (true) {
        switch (_lexer.peekType()) {
            case TokenType::STAR: {
                _lexer.consumeType(TokenType::STAR);
                Expr* rvalue = parsePrefixes(false);
                endPosition = rvalue->endPosition();

                result = new BinaryOperatorExpr(startPosition, endPosition, "*", result, rvalue);
                continue;
            }
            case TokenType::SLASH: {
                _lexer.consumeType(TokenType::SLASH);
                Expr* rvalue = parsePrefixes(false);
                endPosition = rvalue->endPosition();

                result = new BinaryOperatorExpr(startPosition, endPosition, "/", result, rvalue);
                continue;
            }
            case TokenType::PERCENT: {
                _lexer.consumeType(TokenType::PERCENT);
                Expr* rvalue = parsePrefixes(false);
                endPosition = rvalue->endPosition();

                result = new BinaryOperatorExpr(startPosition, endPosition, "%", result, rvalue);
                continue;
            }
            default:
                return result;
        }
    }
}

Expr *Parser::parsePrefixes(bool templateTypingAllowed) {
    TextPosition startPosition = _lexer.peekToken().startPosition;
    TextPosition endPosition;

    switch (_lexer.peekType()) {
        case TokenType::PLUSPLUS: {
            _lexer.consumeType(TokenType::PLUSPLUS);
            Expr* expr = parsePrefixes(false);
            endPosition = expr->endPosition();
            return new PrefixOperatorExpr(startPosition, endPosition, "++", expr);
        }
        case TokenType::MINUSMINUS: {
            _lexer.consumeType(TokenType::MINUSMINUS);
            Expr* expr = parsePrefixes(false);
            endPosition = expr->endPosition();
            return new PrefixOperatorExpr(startPosition, endPosition, "--", expr);
        }
        case TokenType::PLUS: {
            _lexer.consumeType(TokenType::PLUS);
            Expr* expr = parsePrefixes(false);
            endPosition = expr->endPosition();
            return new PrefixOperatorExpr(startPosition, endPosition, "+", expr);
        }
        case TokenType::MINUS: {
            _lexer.consumeType(TokenType::MINUS);
            Expr* expr = parsePrefixes(false);
            endPosition = expr->endPosition();
            return new PrefixOperatorExpr(startPosition, endPosition, "-", expr);
        }
        case TokenType::NOT: {
            _lexer.consumeType(TokenType::NOT);
            Expr* expr = parsePrefixes(false);
            endPosition = expr->endPosition();
            return new PrefixOperatorExpr(startPosition, endPosition, "!", expr);
        }
        case TokenType::TILDE: {
            _lexer.consumeType(TokenType::TILDE);
            Expr* expr = parsePrefixes(false);
            endPosition = expr->endPosition();
            return new PrefixOperatorExpr(startPosition, endPosition, "~", expr);
        }
        case TokenType::STAR: {
            _lexer.consumeType(TokenType::STAR);
            Expr* expr = parsePrefixes(false);
            endPosition = expr->endPosition();
            return new PrefixOperatorExpr(startPosition, endPosition, "*", expr);
        }
        case TokenType::AMPERSAND: {
            _lexer.consumeType(TokenType::AMPERSAND);
            Expr* expr = parsePrefixes(false);
            endPosition = expr->endPosition();
            return new PrefixOperatorExpr(startPosition, endPosition, "&", expr);
        }
        case TokenType::SIZEOF: {
            _lexer.consumeType(TokenType::SIZEOF);
            Expr* expr = parsePrefixes(false);
            endPosition = expr->endPosition();
            return new PrefixOperatorExpr(startPosition, endPosition, "sizeof", expr);
        }
        case TokenType::ALIGNOF: {
            _lexer.consumeType(TokenType::ALIGNOF);
            Expr* expr = parsePrefixes(false);
            endPosition = expr->endPosition();
            return new PrefixOperatorExpr(startPosition, endPosition, "alignof", expr);
        }
        case TokenType::OFFSETOF: {
            _lexer.consumeType(TokenType::OFFSETOF);
            Expr* expr = parsePrefixes(false);
            endPosition = expr->endPosition();
            return new PrefixOperatorExpr(startPosition, endPosition, "offsetof", expr);
        }
        case TokenType::NAMEOF: {
            _lexer.consumeType(TokenType::NAMEOF);
            Expr* expr = parsePrefixes(false);
            endPosition = expr->endPosition();
            return new PrefixOperatorExpr(startPosition, endPosition, "nameof", expr);
        }
        case TokenType::LPAREN: {
            bool oldRightShiftEnabledValue = _lexer.getRightShiftState();
            _lexer.setRightShiftState(true);
            _lexer.consumeType(TokenType::LPAREN);

            Expr* nestedExpr = parseRValue(false);

            endPosition = _lexer.peekToken().endPosition;

            if (!_lexer.consumeType(TokenType::RPAREN)) {
                printError("expected ending ')'! (found '" + _lexer.peekToken().currentSymbol + "')", startPosition, endPosition);
                return nullptr;
            }

            _lexer.setRightShiftState(oldRightShiftEnabledValue);

            // TODO: Support casting
            return new ParenExpr(startPosition, endPosition, nestedExpr);
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
            return parseCallPostfixOrMemberAccess(templateTypingAllowed);
    }
}

Expr *Parser::parseCallPostfixOrMemberAccess(bool templateTypingAllowed) {
    TextPosition startPosition = _lexer.peekToken().startPosition;
    TextPosition endPosition;
    Expr* result = parseVariableLiteralOrParen(templateTypingAllowed);

    while (true) {
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

                while (_lexer.peekType() != TokenType::RPAREN) {
                    arguments.push_back(parseRValue(false));

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

                while (_lexer.peekType() != TokenType::RSQUARE) {
                    arguments.push_back(parseRValue(false));

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

                // TODO: If it's a number literal blah blah
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
}

Expr *Parser::parseVariableLiteralOrParen(bool templateTypingAllowed) {
    Token peekedToken = _lexer.peekToken();
    TextPosition startPosition = peekedToken.startPosition;
    TextPosition endPosition = peekedToken.endPosition;

    switch (peekedToken.tokenType) {
        case TokenType::SYMBOL:
            return parseIdentifier(templateTypingAllowed, false);
        case TokenType::NUMBER:
            return parseNumberLiteral();
        case TokenType::STRING:
            return parseStringLiteral();
        case TokenType::CHARACTER: {
            unsigned int characterValue = peekedToken.currentChar;
            _lexer.consumeType(TokenType::CHARACTER);
            return new CharacterLiteralExpr(startPosition, endPosition, characterValue);
        }
        case TokenType::LPAREN: {
            bool oldRightShiftEnabledValue = _lexer.getRightShiftState();
            _lexer.setRightShiftState(true);
            _lexer.consumeType(TokenType::LPAREN);

            Expr* nestedExpr = parseRValue(false);

            endPosition = _lexer.peekToken().endPosition;

            if (!_lexer.consumeType(TokenType::RPAREN)) {
                printError("expected ending ')'! (found '" + _lexer.peekToken().currentSymbol + "')", startPosition, endPosition);
                return nullptr;
            }

            _lexer.setRightShiftState(oldRightShiftEnabledValue);

            return new ParenExpr(startPosition, endPosition, nestedExpr);
        }
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

    if (STARTS_WITH(peekedToken.currentSymbol, std::string("0x"))) {
        numberBase = 16;
        resultNumber = peekedToken.currentSymbol.substr(2);
        const std::string validCharacters = "0123456789abcdefABCDEF";

        for (const char& c : resultNumber) {
            if (validCharacters.find(c) == std::string::npos) {
                printError(std::string("hexadecimal number literal contains invalid character '") + c + "'!", startPosition, endPosition);
                return nullptr;
            }
        }
    } else if (STARTS_WITH(peekedToken.currentSymbol, std::string("0b"))) {
        numberBase = 2;
        resultNumber = peekedToken.currentSymbol.substr(2);
        const std::string validCharacters = "01";

        for (const char& c : resultNumber) {
            if (validCharacters.find(c) == std::string::npos) {
                printError(std::string("binary number literal contains invalid character '") + c + "'!", startPosition, endPosition);
                return nullptr;
            }
        }
    } else if (STARTS_WITH(peekedToken.currentSymbol, std::string("0"))) {
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

        while (_lexer.peekType() != TokenType::GREATER) {
            templateData.push_back(parseVariableLiteralOrParen(true));

            if (_lexer.peekType() != TokenType::COMMA) break;
            _lexer.consumeType(TokenType::COMMA);
        }

        bool cancelled = true;

        // We don't error out here. If the syntax doesn't match what we expect we just return to our checkpoint.
        if (_lexer.peekType() == TokenType::GREATER) {
            TextPosition potentialNewEndPosition = _lexer.peekToken().endPosition;
            _lexer.consumeType(TokenType::GREATER);

            if ((templateTypingAllowed && _lexer.peekType() == TokenType::SYMBOL)
                || _lexer.peekType() == TokenType::LPAREN
                || _lexer.peekType() == TokenType::RPAREN
                || _lexer.peekType() == TokenType::GREATER
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
