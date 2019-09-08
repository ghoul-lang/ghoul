#ifndef GULC_RETURNSTMT_HPP
#define GULC_RETURNSTMT_HPP

#include <AST/Stmt.hpp>
#include <AST/Expr.hpp>

namespace gulc {
    class ReturnStmt : public Stmt {
    public:
        static bool classof(const Stmt *stmt) { return stmt->getStmtKind() == Kind::Return; }

        ReturnStmt(TextPosition startPosition, TextPosition endPosition, Expr* returnValue = nullptr)
                : Stmt(Kind::Return, startPosition, endPosition), returnValue(returnValue) {}

        Expr* returnValue;
        bool hasReturnValue() const { return returnValue != nullptr; }

        ~ReturnStmt() override {
            delete returnValue;
        }

    private:

    };
}

#endif //GULC_RETURNSTMT_HPP
