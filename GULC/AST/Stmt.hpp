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

#ifndef GULC_STMT_HPP
#define GULC_STMT_HPP

#include <MetaData/TextPosition.hpp>
#include "llvm/Support/Casting.h"

namespace gulc {
    class Stmt {
    public:
        enum class Kind {
            Compound,
            Expr,
            Return,
            Labeled,

            If,
            While,
            For,
            Do,
            Switch,
            Case,

            Break,
            Continue,
            Goto,

            Try,
            TryCatch,
            TryFinally
        };

        Kind getStmtKind() const { return _kind; }
        TextPosition startPosition() const { return _startPosition; }
        TextPosition endPosition() const { return _endPosition; }

        virtual Stmt* deepCopy() const = 0;

        virtual ~Stmt() = default;

    protected:
        Stmt(Kind kind, TextPosition startPosition, TextPosition endPosition)
                : _kind(kind), _startPosition(startPosition), _endPosition(endPosition) {}

    private:
        const Kind _kind;
        const TextPosition _startPosition;
        const TextPosition _endPosition;

    };
}

#endif //GULC_STMT_HPP
