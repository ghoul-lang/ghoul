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

#ifndef GULC_COPYATTR_HPP
#define GULC_COPYATTR_HPP

#include <AST/Attr.hpp>

namespace gulc {
    class CopyAttr : public Attr {
    private:
        // `enabled` is only used when this is applied to an entire struct. If `enabled` is false then the struct
        // is NOT movable. It is copy only
        bool _enabled;

    public:
        static bool classof(const Attr *attr) { return attr->getAttrKind() == Kind::Copy; }

        CopyAttr(TextPosition const& startPosition, TextPosition const& endPosition)
                : CopyAttr(startPosition, endPosition, true) {}

        CopyAttr(TextPosition const& startPosition, TextPosition const& endPosition, bool enabled)
                : Attr(Kind::Copy, "Copy", startPosition, endPosition),
                  _enabled(enabled) {}

        bool enabled() const { return _enabled; }

        Attr* deepCopy() const override {
            return new CopyAttr(startPosition(), endPosition(), _enabled);
        }

    };
}

#endif //GULC_COPYATTR_HPP
