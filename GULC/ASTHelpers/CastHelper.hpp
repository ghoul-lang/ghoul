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

#ifndef GULC_CASTHELPER_HPP
#define GULC_CASTHELPER_HPP

#include <AST/Type.hpp>

namespace gulc {
    enum class CastSide {
        Neither,
        Left,
        Right
    };

    class CastHelper {
    public:
        /**
         * Get which side we should cast
         *
         * @param left
         * @param right
         * @param allowCastOperators - whether or not to allow custom casting functions
         * @return - which side to cast
         */
        static CastSide getSideToCast(BuiltInType* left, BuiltInType* right);

    };
}

#endif //GULC_CASTHELPER_HPP
