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
