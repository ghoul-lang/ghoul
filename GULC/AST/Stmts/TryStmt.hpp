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

#ifndef GULC_TRYSTMT_HPP
#define GULC_TRYSTMT_HPP

#include <AST/Stmt.hpp>
#include <vector>
#include "TryCatchStmt.hpp"
#include "TryFinallyStmt.hpp"
#include "CompoundStmt.hpp"

namespace gulc {
    class TryStmt : public Stmt {
    public:
        static bool classof(const Stmt *stmt) { return stmt->getStmtKind() == Kind::Try; }

        TryStmt(TextPosition startPosition, TextPosition endPosition,
                CompoundStmt* encapsulatedStmt, std::vector<TryCatchStmt*> catchStmts, TryFinallyStmt* finallyStmt)
                : Stmt(Kind::Try, startPosition, endPosition),
                  encapsulatedStmt(encapsulatedStmt), finallyStmt(finallyStmt), _catchStmts(std::move(catchStmts)) {}

        // We require 'CompoundStmt' for 'try', 'catch', and 'finally' to prevent unwanted errors for:
        //
        //     try
        //         // blah...
        //     catch (Exception2 e2)
        //     catch (Exception e)
        //         try
        //             // blah again, use imagination...
        //         catch (IOException ioe)
        //             // blah...
        //         catch (Exception3 e3)
        //
        // In situations similar to that it would be difficult to tell if the nested catch statements are nested
        // or attached to the first try statement
        CompoundStmt* encapsulatedStmt;
        std::vector<TryCatchStmt*>& catchStmts() { return _catchStmts; }
        const std::vector<TryCatchStmt*>& catchStmts() const { return _catchStmts; }
        TryFinallyStmt* finallyStmt;

        bool hasCatchStmts() const { return !_catchStmts.empty(); }
        bool hasFinallyStmt() const { return finallyStmt != nullptr; }

        Stmt* deepCopy() const override {
            std::vector<TryCatchStmt*> copiedCatchStmts;

            for (TryCatchStmt* catchStmt : _catchStmts) {
                copiedCatchStmts.push_back(static_cast<TryCatchStmt*>(catchStmt->deepCopy()));
            }

            return new TryStmt(startPosition(), endPosition(),
                               static_cast<CompoundStmt*>(encapsulatedStmt->deepCopy()),
                               std::move(copiedCatchStmts),
                               static_cast<TryFinallyStmt*>(finallyStmt->deepCopy()));
        }

        ~TryStmt() override {
            for (TryCatchStmt* tryCatchStmt : _catchStmts) {
                delete tryCatchStmt;
            }

            delete encapsulatedStmt;
            delete finallyStmt;
        }

    private:
        std::vector<TryCatchStmt*> _catchStmts;

    };
}

#endif //GULC_TRYSTMT_HPP
