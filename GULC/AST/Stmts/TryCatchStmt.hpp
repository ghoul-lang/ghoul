#ifndef GULC_TRYCATCHSTMT_HPP
#define GULC_TRYCATCHSTMT_HPP

#include <AST/Stmt.hpp>
#include <AST/Expr.hpp>
#include "CompoundStmt.hpp"

namespace gulc {
    /// The 'catch' aspect of the 'try' Stmt
    class TryCatchStmt : public Stmt {
    public:
        static bool classof(const Stmt *stmt) { return stmt->getStmtKind() == StmtKind::TryCatch; }

        TryCatchStmt(TextPosition startPosition, TextPosition endPosition,
                     Expr* exceptionDecl, CompoundStmt* handlerStmt)
                : Stmt(StmtKind::TryCatch, startPosition, endPosition),
                  _exceptionDecl(exceptionDecl), _handlerStmt(handlerStmt) {}

        const Expr* exceptionDecl() const { return _exceptionDecl; }
        const CompoundStmt* handlerStmt() const { return _handlerStmt; }
        bool hasExceptionDecl() const { return _exceptionDecl != nullptr; }

        ~TryCatchStmt() override {
            delete _exceptionDecl;
            delete _handlerStmt;
        }

    private:
        Expr* _exceptionDecl;
        CompoundStmt* _handlerStmt;

    };
}

#endif //GULC_TRYCATCHSTMT_HPP
