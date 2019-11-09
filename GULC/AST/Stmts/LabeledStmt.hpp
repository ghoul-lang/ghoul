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

#ifndef GULC_LABELEDSTMT_HPP
#define GULC_LABELEDSTMT_HPP

#include <AST/Stmt.hpp>
#include <string>

namespace gulc {
    class LabeledStmt : public Stmt {
    public:
        static bool classof(const Stmt *stmt) { return stmt->getStmtKind() == Kind::Labeled; }

        LabeledStmt(TextPosition startPosition, TextPosition endPosition, std::string label, Stmt* labeledStmt)
                : Stmt(Kind::Labeled, startPosition, endPosition),
                  labeledStmt(labeledStmt), _label(std::move(label)) {}

        std::string label() const { return _label; }
        Stmt* labeledStmt;

        Stmt* deepCopy() const override {
            auto result = new LabeledStmt(startPosition(), endPosition(), _label,
                                          labeledStmt->deepCopy());
            result->isUnreachable = isUnreachable;
            return result;
        }

        // This is used by the passes to store the number of local variables that were declared before the label
        unsigned int currentNumLocalVariables;

        ~LabeledStmt() override {
            delete labeledStmt;
        }

    private:
        std::string _label;

    };
}

#endif //GULC_LABELEDSTMT_HPP
