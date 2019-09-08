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

        ~TryFinallyStmt() override {
            delete handlerStmt;
        }

    };
}

#endif //GULC_TRYFINALLYSTMT_HPP
