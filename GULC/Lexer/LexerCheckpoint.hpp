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
