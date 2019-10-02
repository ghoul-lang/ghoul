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

#ifndef GULC_TOKENTYPE_HPP
#define GULC_TOKENTYPE_HPP

namespace gulc {
	enum class TokenType {
		NIL,
		// Value Tokens
		NUMBER, // Ex: 0xAF, 12, 07, 0b1101 - Decimals should be handled by the parser
		CHARACTER, // Ex: b, c, \t, \n, \0
		STRING, // Ex: "hello\t\n", "Здравствуйте", "您好", etc. (UTF-8 encoded?)
		SYMBOL,

		// Keyword Tokens
		INTERFACE, // 'interface'
		STRUCT,
		CLASS,
		UNION,
		ENUM,
		OPERATOR,
		INFIX,
		PREFIX,
		EXPLICIT,
		IMPLICIT,
		NAMESPACE,
		SIZEOF,
		ALIGNOF,
		OFFSETOF,
		NAMEOF,
		IF,
		ELSE,
		DO,
		WHILE,
		FOR,
		SWITCH,
		CASE,
		DEFAULT,
		CONTINUE,
		BREAK,
		GOTO,
		RETURN,
		ASM,
		IMPORT,
		AS, // Used for 'import foo as bar;'
		TRY,
		CATCH,
		FINALLY,
		MODIFIER, // TODO: Should we do this?
		KEYWORD,
		CHECK,

		// Modifier Tokens
		PUBLIC, // 'public'
		PRIVATE, // 'private'
		PROTECTED, // 'protected'
		INTERNAL, // 'internal'
		STATIC, // 'static'
		CONST, // 'const'
		MUT, // `mut`
		IMMUT, // `immut`
		EXTERN, // 'extern'
		VOLATILE, // 'volatile'
		ABSTRACT, // 'abstract'
		SEALED, // 'sealed'
		VIRTUAL, // 'virtual'
		OVERRIDE, // 'override'
		IN, // 'in'
		OUT, // 'out'
		REF, // 'ref'

		// Operator Tokens
		EQUALS, // '='
		EQUALEQUALS, // '=='
		TEMPLATEEND, // '>' when right shift is disabled
		GREATER, // '>'
		GREATEREQUALS, // '>='
		RIGHT, // '>>'
		RIGHTEQUALS, // '>>='
		LESS, // '<'
		LESSEQUALS, // '<='
		LEFT, // '<<'
		LEFTEQUALS, // '<<='
		NOT, // '!'
		NOTEQUALS, // '!='
		TILDE, // '~'
		PLUS, // '+'
		PLUSEQUALS, // '+='
		PLUSPLUS, // '++'
		MINUS, // '-'
		MINUSEQUALS, // '-='
		MINUSMINUS, // '--'
		STAR, // '*'
		STAREQUALS, // '*='
		SLASH, // '/'
		SLASHEQUALS, // '/='
		PERCENT, // '%'
		PERCENTEQUALS, // '%='
		AMPERSAND, // '&'
		AMPERSANDEQUALS, // '&='
		AMPERSANDAMPERSAND, // '&&'
		PIPE, // '|'
		PIPEEQUALS, // '|='
		PIPEPIPE, // '||'
		CARET, // '^'
		CARETEQUALS, // '^='
		PERIOD, // '.'
		ARROW, // '->'
		COLON, // ':'
		QUESTION, // '?'

		// Special Tokens
		LCURLY, // '{'
		RCURLY, // '}'
		LSQUARE, // '['
		RSQUARE, // ']'
		SEMICOLON, // ';'
		COMMA, // ','
		LPAREN, // '('
		RPAREN, // ')'
		// TODO: Probably not needed.
		ENDOFFILE // might be removed, just used to double check everything is parsed correctly
	};
}

#endif //GULC_TOKENTYPE_HPP
