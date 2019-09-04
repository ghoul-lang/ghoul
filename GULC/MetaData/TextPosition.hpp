#ifndef GULC_TEXTPOSITION_HPP
#define GULC_TEXTPOSITION_HPP

namespace gulc {
	struct TextPosition {
		unsigned int index;
		unsigned int line;
		unsigned int column;

		TextPosition()
			: index(0), line(0), column(0) {}

		TextPosition(unsigned int index, unsigned int line, unsigned int column)
			: index(index), line(line), column(column) {}
	};
}

#endif //GULC_TEXTPOSITION_HPP
