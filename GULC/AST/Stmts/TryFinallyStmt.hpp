#ifndef GULC_TRYFINALLYSTMT_HPP
#define GULC_TRYFINALLYSTMT_HPP

#include <AST/Stmt.hpp>
#include "CompoundStmt.hpp"

namespace gulc {
    /// The 'finally' aspect of the 'try' Stmt
    class TryFinallyStmt : public Stmt {
    public:
        static bool classof(const Stmt *stmt) { return stmt->getStmtKind() == StmtKind::TryFinally; }

        TryFinallyStmt(TextPosition startPosition, TextPosition endPosition, CompoundStmt* handlerStmt)
                : Stmt(StmtKind::TryFinally, startPosition, endPosition),
                  _handlerStmt(handlerStmt) {}

        const CompoundStmt* handlerStmt() const { return _handlerStmt; }

        ~TryFinallyStmt() override {
            delete _handlerStmt;
        }

    private:
        CompoundStmt* _handlerStmt;

    };
}

#endif //GULC_TRYFINALLYSTMT_HPP
