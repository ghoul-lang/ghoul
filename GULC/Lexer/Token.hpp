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

#ifndef GULC_TOKEN_HPP
#define GULC_TOKEN_HPP

#include <string>
#include "TokenType.hpp"
#include "TokenMetaType.hpp"
#include "MetaData/TextPosition.hpp"

namespace gulc {
	struct Token {
		TokenType tokenType;
		//TokenMetaType tokMetaType;
		TokenMetaType metaType;
		// TODO: Decide if these should be put in a union?
		std::string currentSymbol;
		unsigned int currentChar;
		TextPosition startPosition;
		TextPosition endPosition;

		Token(TokenType tokenType, TokenMetaType metaType, std::string currentSymbol, unsigned int currentChar,
              TextPosition startPosition, TextPosition endPosition)
                : tokenType(tokenType), metaType(metaType), currentSymbol(std::move(currentSymbol)),
                  currentChar(currentChar), startPosition(startPosition), endPosition(endPosition) {
			// endPosition.column comes in as an inclusive value, make it exclusive
			//this->endPosition.column += 1;
		}
	};
}

#endif //GULC_TOKEN_HPP
