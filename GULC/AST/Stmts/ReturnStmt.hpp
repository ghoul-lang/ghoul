#ifndef GULC_RETURNSTMT_HPP
#define GULC_RETURNSTMT_HPP

#include <AST/Stmt.hpp>
#include <AST/Expr.hpp>

namespace gulc {
    class ReturnStmt : public Stmt {
    public:
        static bool classof(const Stmt *stmt) { return stmt->getStmtKind() == StmtKind::Return; }

        ReturnStmt(TextPosition startPosition, TextPosition endPosition, Expr* returnValue = nullptr)
                : Stmt(StmtKind::Return, startPosition, endPosition), _returnValue(returnValue) {}

        const Expr* returnValue() const { return _returnValue; }

        ~ReturnStmt() override {
            delete _returnValue;
        }

    private:
        const Expr* _returnValue;

    };
}

#endif //GULC_RETURNSTMT_HPP
