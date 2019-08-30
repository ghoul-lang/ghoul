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
			this->endPosition.column += 1;
		}
	};
}

#endif //GULC_TOKEN_HPP
