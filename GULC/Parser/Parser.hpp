#ifndef GULC_PARSER_HPP
#define GULC_PARSER_HPP

#include <string>
#include <Lexer/Lexer.hpp>
#include <AST/Decl.hpp>
#include <vector>
#include <AST/Decls/ParameterDecl.hpp>
#include <AST/Stmts/CompoundStmt.hpp>
#include <AST/Stmts/ReturnStmt.hpp>
#include <AST/Exprs/IdentifierExpr.hpp>

namespace gulc {
    class Parser {
    public:
        explicit Parser(std::string filePath);

        Decl* parseTopLevelDecl();

    private:
        std::string _filePath;
        gulc::Lexer _lexer;

        void printError(const std::string& errorMessage, TextPosition startPosition, TextPosition endPosition);
        void printWarning(const std::string& warningMessage, TextPosition startPosition, TextPosition endPosition);

        std::vector<ParameterDecl*> parseParameterDecls(TextPosition startPosition);

        Type* parseType();

        Stmt* parseStmt();
        CompoundStmt* parseCompoundStmt();
        ReturnStmt* parseReturnStmt();

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
        Expr* parseRValue(bool templateTypingAllowed);
        Expr* parseAssignmentMisc(bool templateTypingAllowed);
        Expr* parseLogicalOr(bool templateTypingAllowed);
        Expr* parseLogicalAnd(bool templateTypingAllowed);
        Expr* parseBitwiseOr(bool templateTypingAllowed);
        Expr* parseBitwiseXor(bool templateTypingAllowed);
        Expr* parseBitwiseAnd(bool templateTypingAllowed);
        Expr* parseEqualToNotEqualTo(bool templateTypingAllowed);
        Expr* parseGreaterThanLessThan(bool templateTypingAllowed);
        Expr* parseBitwiseShifts(bool templateTypingAllowed);
        // TODO: Verify everything is parsed in the correct order (make sure it's ((12 / 2) / 2) instead of (12 / (2 / 2)))
        Expr* parseAdditionSubtraction(bool templateTypingAllowed);
        Expr* parseMultiplicationDivisionOrRemainder(bool templateTypingAllowed);
        // TODO: We might want to add a function that is called before this one that will handle custom operators. It should use loop as long as PeekType is SYMBOL
        Expr* parsePrefixes(bool templateTypingAllowed);
        Expr* parseCallPostfixOrMemberAccess(bool templateTypingAllowed);
        Expr* parseVariableLiteralOrParen(bool templateTypingAllowed);
        Expr* parseNumberLiteral();
        Expr* parseStringLiteral();
        IdentifierExpr* parseIdentifier(bool templateTypingAllowed, bool ignoreGenerics = false);

    };
}

#endif //GULC_PARSER_HPP
