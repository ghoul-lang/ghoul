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

#ifndef GULC_GOTOSTMT_HPP
#define GULC_GOTOSTMT_HPP

#include <AST/Stmt.hpp>
#include <string>
#include <AST/Expr.hpp>
#include <vector>

namespace gulc {
    class GotoStmt : public Stmt {
    public:
        static bool classof(const Stmt *stmt) { return stmt->getStmtKind() == Kind::Goto; }

        GotoStmt(TextPosition startPosition, TextPosition endPosition, std::string label)
                : Stmt(Kind::Goto, startPosition, endPosition),
                  label(std::move(label)) {}

        std::string label;

        Stmt* deepCopy() const override {
            std::vector<Expr*> copiedPreGotoCleanup;

            for (Expr* preContinueCleanupExpr : preGotoCleanup) {
                copiedPreGotoCleanup.push_back(preContinueCleanupExpr->deepCopy());
            }

            auto result = new GotoStmt(startPosition(), endPosition(), label);
            result->preGotoCleanup = std::move(copiedPreGotoCleanup);
            return result;
        }

        std::vector<Expr*> preGotoCleanup;

        // This is used by the passes to store the number of local variables that exist in the context of the goto
        unsigned int currentNumLocalVariables;

    };
}

#endif //GULC_GOTOSTMT_HPP
