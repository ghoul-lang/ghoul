#ifndef GULC_EXPLICITCASTEXPR_HPP
#define GULC_EXPLICITCASTEXPR_HPP

#include <AST/Expr.hpp>
#include <AST/Type.hpp>

namespace gulc {
    class ExplicitCastExpr : public Expr {
    public:
        static bool classof(const Expr *expr) { return expr->getExprKind() == Kind::ExplicitCast; }

        ExplicitCastExpr(TextPosition startPosition, TextPosition endPosition,
                         Type* castType, Expr* castee)
                : Expr(Kind::ExplicitCast, startPosition, endPosition),
                  castType(castType), castee(castee) {}

        Type* castType;
        Expr* castee;

        ~ExplicitCastExpr() override {
            delete castType;
            delete castee;
        }

    };
}

#endif //GULC_EXPLICITCASTEXPR_HPP
