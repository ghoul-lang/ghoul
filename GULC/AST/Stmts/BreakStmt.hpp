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

#ifndef GULC_BREAKSTMT_HPP
#define GULC_BREAKSTMT_HPP

#include <AST/Stmt.hpp>
#include <string>
#include <vector>
#include <AST/Expr.hpp>

namespace gulc {
    class BreakStmt : public Stmt {
    public:
        static bool classof(const Stmt *stmt) { return stmt->getStmtKind() == Kind::Break; }

        BreakStmt(TextPosition startPosition, TextPosition endPosition, std::string label)
                : Stmt(Kind::Break, startPosition, endPosition),
                  _label(std::move(label)) {}

        std::string label() const { return _label; }

        Stmt* deepCopy() const override {
            std::vector<Expr*> copiedPreBreakCleanup;

            for (Expr* preBreakCleanupExpr : preBreakCleanup) {
                copiedPreBreakCleanup.push_back(preBreakCleanupExpr->deepCopy());
            }

            auto result = new BreakStmt(startPosition(), endPosition(), _label);
            result->preBreakCleanup = std::move(copiedPreBreakCleanup);
            result->isUnreachable = isUnreachable;
            return result;
        }

        std::vector<Expr*> preBreakCleanup;

        ~BreakStmt() override {
            for (Expr* preBreakCleanupExpr : preBreakCleanup) {
                delete preBreakCleanupExpr;
            }
        }

    private:
        std::string _label;

    };
}

#endif //GULC_BREAKSTMT_HPP
