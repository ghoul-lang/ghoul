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

#ifndef GULC_CONTINUESTMT_HPP
#define GULC_CONTINUESTMT_HPP

#include <AST/Stmt.hpp>
#include <string>
#include <vector>
#include <AST/Expr.hpp>

namespace gulc {
    class ContinueStmt : public Stmt {
    public:
        static bool classof(const Stmt *stmt) { return stmt->getStmtKind() == Kind::Continue; }

        ContinueStmt(TextPosition startPosition, TextPosition endPosition, std::string label)
                : Stmt(Kind::Continue, startPosition, endPosition),
                  _label(std::move(label)) {}

        std::string label() const { return _label; }

        Stmt* deepCopy() const override {
            std::vector<Expr*> copiedPreContinueCleanup;

            for (Expr* preContinueCleanupExpr : preContinueCleanup) {
                copiedPreContinueCleanup.push_back(preContinueCleanupExpr->deepCopy());
            }

            auto result = new ContinueStmt(startPosition(), endPosition(), _label);
            result->preContinueCleanup = std::move(copiedPreContinueCleanup);
            return result;
        }

        std::vector<Expr*> preContinueCleanup;

        ~ContinueStmt() override {
            for (Expr* preContinueCleanupExpr : preContinueCleanup) {
                delete preContinueCleanupExpr;
            }
        }

    private:
        std::string _label;

    };
}

#endif //GULC_CONTINUESTMT_HPP
