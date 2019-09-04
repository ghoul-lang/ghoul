#ifndef GULC_LEXER_HPP
#define GULC_LEXER_HPP

#include <string>
#include "TokenType.hpp"
#include "TokenMetaType.hpp"
#include "Token.hpp"
#include "MetaData/TextPosition.hpp"
#include "LexerCheckpoint.hpp"

namespace gulc {
	class Lexer {
    public:
	    Lexer() = default;
        Lexer(std::string filePath, std::string sourceCode)
                : _filePath(std::move(filePath)), _sourceCode(std::move(sourceCode)) { }

        TokenType peekType();
        TokenMetaType peekMeta();
        Token peekToken();
        Token nextToken();
        bool consumeType(TokenType type);

        LexerCheckpoint createCheckpoint();
        void returnToCheckpoint(const LexerCheckpoint& checkpoint);

        bool getRightShiftState();
        void setRightShiftState(bool enabled);

    private:
        std::string _filePath;
        std::string _sourceCode;
        Token _nextToken = Token(TokenType::NIL, TokenMetaType::NIL, std::string(), 0, TextPosition(0, 0, 0), TextPosition(0, 0, 0));
        unsigned int _currentLine = 1;
        unsigned int _currentColumn = 1;
        unsigned int _currentIndex = 0;
        // We support disabling this so we can do 'GenericType1<GenericType2<GenericType3<int>>>' easily
        // This will make it so the last three '>' characters will come in as separate tokens rather than coming in as one '>' token and one '>>' token
        bool _rightShiftEnabled = true;

        Token lexOneToken();
        Token parseToken(std::string& tokenText, TextPosition startPosition);

        void printErrorAndExit(const std::string& errorText, int errorCode = 1);
        void errorUnexpectedEOF();
	};
}

#endif //GULC_LEXER_HPP
