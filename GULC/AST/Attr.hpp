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

#ifndef GULC_ATTR_HPP
#define GULC_ATTR_HPP

#include <string>
#include <MetaData/TextPosition.hpp>

namespace gulc {
    class Attr {
    public:
        enum class Kind {
            // References an `AttributeDecl`...
            Custom,
            // An unresolved attribute. Could be `Move` or something custom
            Unresolved,

            Move,
            Copy,
            Pod
        };

        Kind getAttrKind() const { return _kind; }
        std::string name() const { return _name; }
        TextPosition startPosition() const { return _startPosition; }
        TextPosition endPosition() const { return _endPosition; }

        virtual Attr* deepCopy() const = 0;

        virtual ~Attr() = default;

    protected:
        Attr(Kind kind, std::string name, TextPosition startPosition, TextPosition endPosition)
                : _kind(kind), _name(std::move(name)), _startPosition(startPosition), _endPosition(endPosition) {}

    private:
        const Kind _kind;
        const std::string _name;
        const TextPosition _startPosition;
        const TextPosition _endPosition;

    };
}

#endif //GULC_ATTR_HPP
