#ifndef GULC_EXPR_HPP
#define GULC_EXPR_HPP

#include "Stmt.hpp"
#include "Type.hpp"

namespace gulc {
    class Expr : public Stmt {
    public:
        static bool classof(const Stmt *stmt) { return stmt->getStmtKind() == Stmt::Kind::Expr; }

        enum class Kind {
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
            ExplicitCast,
            ImplicitCast,

            LocalVariableDeclOrPrefixOperatorCallExpr,

            ResolvedTypeRef,
            UnresolvedTypeRef,

            LocalVariableDecl
        };

        Kind getExprKind() const { return _kind; }

        // This is the type of this expression. I.e `(float)i + 1` will make the resultType of the 'BinaryOperatorExpr' 'float'
        Type* resultType;

        ~Expr() override {
            delete resultType;
        }

    protected:
        Expr(Kind kind, TextPosition startPosition, TextPosition endPosition)
                : Stmt(Stmt::Kind::Expr, startPosition, endPosition), _kind(kind), resultType(nullptr) {}

    private:
        const Kind _kind;

    };
}

#endif //GULC_EXPR_HPP
