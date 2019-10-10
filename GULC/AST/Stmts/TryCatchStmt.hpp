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

#ifndef GULC_TRYCATCHSTMT_HPP
#define GULC_TRYCATCHSTMT_HPP

#include <AST/Stmt.hpp>
#include <AST/Expr.hpp>
#include "CompoundStmt.hpp"
#include <string>

namespace gulc {
    /// The 'catch' aspect of the 'try' Stmt
    class TryCatchStmt : public Stmt {
    public:
        static bool classof(const Stmt *stmt) { return stmt->getStmtKind() == Kind::TryCatch; }

        TryCatchStmt(TextPosition startPosition, TextPosition endPosition,
                     Type* exceptionType, std::string exceptionVarName, CompoundStmt* handlerStmt)
                : Stmt(Kind::TryCatch, startPosition, endPosition),
                  exceptionType(exceptionType), exceptionVarName(std::move(exceptionVarName)),
                  handlerStmt(handlerStmt) {}

        Type* exceptionType;
        std::string exceptionVarName;
        CompoundStmt* handlerStmt;
        bool hasExceptionDecl() const { return exceptionType != nullptr; }

        ~TryCatchStmt() override {
            delete exceptionType;
            delete handlerStmt;
        }

    };
}

#endif //GULC_TRYCATCHSTMT_HPP
