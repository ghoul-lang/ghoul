#ifndef GULC_LEXERCHECKPOINT_HPP
#define GULC_LEXERCHECKPOINT_HPP

#include "Token.hpp"
#include "MetaData/TextPosition.hpp"

namespace gulc {
    struct LexerCheckpoint {
        Token _nextToken = Token(TokenType::NIL, TokenMetaType::NIL, nullptr, 0, TextPosition(0, 0, 0), TextPosition(0, 0, 0));
        unsigned int currentLine = 1;
        unsigned int currentColumn = 0;
        unsigned int currentIndex = 0;

        LexerCheckpoint(Token nextToken,
                        unsigned int currentLine, unsigned int currentColumn, unsigned int currentIndex)
                : _nextToken(std::move(nextToken)),
                  currentLine(currentLine), currentColumn(currentColumn), currentIndex(currentIndex) { }
    };
}

#endif //GULC_LEXERCHECKPOINT_HPP
