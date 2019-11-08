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

#ifndef GULC_FORSTMT_HPP
#define GULC_FORSTMT_HPP

#include <AST/Stmt.hpp>
#include <AST/Expr.hpp>
#include <vector>

namespace gulc {
    class ForStmt : public Stmt {
    public:
        static bool classof(const Stmt *stmt) { return stmt->getStmtKind() == Kind::For; }

        ForStmt(TextPosition startPosition, TextPosition endPosition,
                Expr* preLoop, Expr* condition, Expr* iterationExpr, Stmt* loopStmt)
                : Stmt(Kind::For, startPosition, endPosition),
                  preLoop(preLoop), condition(condition), iterationExpr(iterationExpr), loopStmt(loopStmt) {}

        Expr* preLoop;
        Expr* condition;
        /// The expression that gets called every loop
        Expr* iterationExpr;
        Stmt* loopStmt;
        // Expressions for cleaning up the preloop variables
        std::vector<Expr*> postLoopCleanup;

        Stmt* deepCopy() const override {
            std::vector<Expr*> copiedPostLoopCleanup{};

            for (Expr* preLoopCleanupExpr : postLoopCleanup) {
                copiedPostLoopCleanup.push_back(preLoopCleanupExpr->deepCopy());
            }

            auto result = new ForStmt(startPosition(), endPosition(), preLoop->deepCopy(),
                                      condition->deepCopy(), iterationExpr->deepCopy(),
                                      loopStmt->deepCopy());

            result->postLoopCleanup = std::move(copiedPostLoopCleanup);

            return result;
        }

        // This is used by the passes to store the number of local variables that exist in the context of the for loop
        // This SHOULD contain the number of variables declared in `preLoop`
        unsigned int currentNumLocalVariables;

        ~ForStmt() override {
            delete preLoop;
            delete condition;
            delete iterationExpr;
            delete loopStmt;
        }

    };
}

#endif //GULC_FORSTMT_HPP
