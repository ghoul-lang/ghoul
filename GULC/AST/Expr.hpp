#ifndef GULC_EXPR_HPP
#define GULC_EXPR_HPP

#include "Stmt.hpp"

namespace gulc {
    class Expr : public Stmt {
    public:
        static bool classof(const Stmt *stmt) { return stmt->getStmtKind() == StmtKind::Expr; }

        enum class ExprKind {
            BinaryOperator,
            PostfixOperator,
            PrefixOperator,

            Ternary,

            FunctionCall,
            IndexerCall,
            MemberAccessCall,

            Identifier,

            Paren,

            IntegerLiteral,
            FloatLiteral,
            StringLiteral,
            CharacterLiteral,

            PotentialExplicitCast,

            LocalVariableDeclOrPrefixOperatorCallExpr
        };

        ExprKind getExprKind() const { return _kind; }

    protected:
        Expr(ExprKind kind, TextPosition startPosition, TextPosition endPosition)
                : Stmt(StmtKind::Expr, startPosition, endPosition), _kind(kind) {}

    private:
        const ExprKind _kind;

    };
}

#endif //GULC_EXPR_HPP
