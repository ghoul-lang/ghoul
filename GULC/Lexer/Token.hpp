#pragma once

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
		std::string* currentSymbol;
		unsigned int currentChar;
		TextPosition startPosition;
		TextPosition endPosition;

		Token(TokenType tokenType, TokenMetaType metaType, std::string* currentSymbol, unsigned int currentChar,
				TextPosition startPosition, TextPosition endPosition) {
			this->tokenType = tokenType;
			this->metaType = metaType;
			this->currentSymbol = currentSymbol;
			this->currentChar = currentChar;
			this->startPosition = startPosition;
			this->endPosition = endPosition;
			// endPosition.column comes in as an inclusive value, make it exclusive
			this->endPosition.column += 1;
		}
	};
}
