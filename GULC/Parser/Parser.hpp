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

#ifndef GULC_PARSER_HPP
#define GULC_PARSER_HPP

#include <string>
#include <Lexer/Lexer.hpp>
#include <AST/Decl.hpp>
#include <vector>
#include <AST/Decls/ParameterDecl.hpp>
#include <AST/Decls/TemplateParameterDecl.hpp>
#include <AST/Stmts/CompoundStmt.hpp>
#include <AST/Stmts/ReturnStmt.hpp>
#include <AST/Exprs/IdentifierExpr.hpp>
#include <AST/Stmts/IfStmt.hpp>
#include <AST/Stmts/WhileStmt.hpp>
#include <AST/Stmts/ForStmt.hpp>
#include <AST/Stmts/DoStmt.hpp>
#include <AST/Stmts/SwitchStmt.hpp>
#include <AST/Stmts/BreakStmt.hpp>
#include <AST/Stmts/ContinueStmt.hpp>
#include <AST/Stmts/GotoStmt.hpp>
#include <AST/Stmts/TryStmt.hpp>
#include <AST/FileAST.hpp>

namespace gulc {
    class Parser {
    public:
        explicit Parser(std::string filePath);

        FileAST parseFile();

    private:
        std::string _filePath;
        gulc::Lexer _lexer;

        void printError(const std::string& errorMessage, TextPosition startPosition, TextPosition endPosition);
        void printWarning(const std::string& warningMessage, TextPosition startPosition, TextPosition endPosition);

        Decl* parseTopLevelDecl();
        std::vector<TemplateParameterDecl*> parseTemplateParameterDecls(TextPosition startPosition);
        std::vector<ParameterDecl*> parseParameterDecls(TextPosition startPosition);

        Type* parseType(bool parseSuffix = true);
        Type* parseTypeSuffix(Type* type);

        Stmt* parseStmt();
        CompoundStmt* parseCompoundStmt();
        ReturnStmt* parseReturnStmt();
        IfStmt* parseIfStmt();
        WhileStmt* parseWhileStmt();
        ForStmt* parseForStmt();
        DoStmt* parseDoStmt();
        SwitchStmt* parseSwitchStmt();
        BreakStmt* parseBreakStmt();
        ContinueStmt* parseContinueStmt();
        GotoStmt* parseGotoStmt();
        TryStmt* parseTryStmt();

        /*
         * "templateTypingAllowed" allows us to parse 'Template<int>' alone.
         * When it's false we still allow for "Function<int>()" and "(Cast<int>)"
         * This is a really hacky way of solving the issue with the below syntax:
         *
         *     functionCall(lBool < rBool, integerValue, lBool > rBool);
         *
         * When "templateTypingAllowed" is false it will treat the '<' and '>' as greater and less than
         * If "templateTypingAllowed" was set to true for that (it should NOT be) -
         * then the parser will think "lBool<rBool, integerValue, lBool> rBool" is a variable declaration within the -
         * functionCall
         */
        // TODO: Is this a good name for what this is?
        Expr* parseExpr(bool isStatement, bool templateTypingAllowed);
        Expr* parseAssignmentMisc(bool isStatement, bool templateTypingAllowed);
        Expr* parseLogicalOr(bool isStatement, bool templateTypingAllowed);
        Expr* parseLogicalAnd(bool isStatement, bool templateTypingAllowed);
        Expr* parseBitwiseOr(bool isStatement, bool templateTypingAllowed);
        Expr* parseBitwiseXor(bool isStatement, bool templateTypingAllowed);
        Expr* parseBitwiseAnd(bool isStatement, bool templateTypingAllowed);
        Expr* parseEqualToNotEqualTo(bool isStatement, bool templateTypingAllowed);
        Expr* parseGreaterThanLessThan(bool isStatement, bool templateTypingAllowed);
        Expr* parseBitwiseShifts(bool isStatement, bool templateTypingAllowed);
        Expr* parseAdditionSubtraction(bool isStatement, bool templateTypingAllowed);
        Expr* parseMultiplicationDivisionOrRemainder(bool isStatement, bool templateTypingAllowed);
        // TODO: We might want to add a function that is called before this one that will handle custom operators. It should use loop as long as PeekType is SYMBOL
        Expr* parsePrefixes(bool isStatement, bool templateTypingAllowed);
        Expr* parseCallPostfixOrMemberAccess(bool isStatement, bool templateTypingAllowed);
        Expr* parseVariableLiteralOrParen(bool isStatement, bool templateTypingAllowed);
        Expr* parseNumberLiteral();
        Expr* parseStringLiteral();
        IdentifierExpr* parseIdentifier(bool templateTypingAllowed, bool ignoreGenerics = false);

    };
}

#endif //GULC_PARSER_HPP
