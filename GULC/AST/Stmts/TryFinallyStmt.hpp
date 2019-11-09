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

#ifndef GULC_TRYFINALLYSTMT_HPP
#define GULC_TRYFINALLYSTMT_HPP

#include <AST/Stmt.hpp>
#include "CompoundStmt.hpp"

namespace gulc {
    /// The 'finally' aspect of the 'try' Stmt
    class TryFinallyStmt : public Stmt {
    public:
        static bool classof(const Stmt *stmt) { return stmt->getStmtKind() == Kind::TryFinally; }

        TryFinallyStmt(TextPosition startPosition, TextPosition endPosition, CompoundStmt* handlerStmt)
                : Stmt(Kind::TryFinally, startPosition, endPosition),
                  handlerStmt(handlerStmt) {}

        CompoundStmt* handlerStmt;

        Stmt* deepCopy() const override {
            auto result = new TryFinallyStmt(startPosition(), endPosition(),
                                             static_cast<CompoundStmt*>(handlerStmt->deepCopy()));
            result->isUnreachable = isUnreachable;
            return result;
        }

        ~TryFinallyStmt() override {
            delete handlerStmt;
        }

    };
}

#endif //GULC_TRYFINALLYSTMT_HPP
