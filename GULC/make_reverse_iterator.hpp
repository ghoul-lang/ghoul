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

#ifndef GULC_MAKE_REVERSE_ITERATOR_HPP
#define GULC_MAKE_REVERSE_ITERATOR_HPP

namespace gulc {
    template<typename T>
    struct reverse_iterator_wrapper {
        T& _iterator;

        auto begin() {
            return _iterator.rbegin();
        }

        auto end() {
            return _iterator.rend();
        }
    };

    template<typename T>
    reverse_iterator_wrapper<T> reverse(T&& iterator) {
        return reverse_iterator_wrapper<T> {
                iterator
        };
    }
}

#endif //GULC_MAKE_REVERSE_ITERATOR_HPP
