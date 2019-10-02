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

#ifndef GULC_COMPOUNDSTMT_HPP
#define GULC_COMPOUNDSTMT_HPP

#include <AST/Stmt.hpp>
#include <vector>

namespace gulc {
    class CompoundStmt : public Stmt {
    public:
        static bool classof(const Stmt *stmt) { return stmt->getStmtKind() == Kind::Compound; }

        CompoundStmt(TextPosition startPosition, TextPosition endPosition, std::vector<Stmt*> statements)
                : Stmt(Kind::Compound, startPosition, endPosition), _statements(std::move(statements)) {}

        const std::vector<Stmt*>& statements() const { return _statements; }
        std::vector<Stmt*>& statements() { return _statements; }

    private:
        std::vector<Stmt*> _statements;

    };
}

#endif //GULC_COMPOUNDSTMT_HPP
