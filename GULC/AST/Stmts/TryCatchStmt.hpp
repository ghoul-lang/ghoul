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
                     Expr* exceptionType, std::string exceptionVarName, CompoundStmt* handlerStmt)
                : Stmt(Kind::TryCatch, startPosition, endPosition),
                  exceptionType(exceptionType), exceptionVarName(std::move(exceptionVarName)),
                  handlerStmt(handlerStmt) {}

        Expr* exceptionType;
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
