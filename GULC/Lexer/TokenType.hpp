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
