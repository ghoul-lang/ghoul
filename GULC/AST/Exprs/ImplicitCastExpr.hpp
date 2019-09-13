#ifndef GULC_IMPLICITCASTEXPR_HPP
#define GULC_IMPLICITCASTEXPR_HPP

#include <AST/Expr.hpp>
#include <AST/Type.hpp>

namespace gulc {
    class ImplicitCastExpr : public Expr {
    public:
        static bool classof(const Expr *expr) { return expr->getExprKind() == Kind::ImplicitCast; }

        ImplicitCastExpr(TextPosition startPosition, TextPosition endPosition,
                         Type* castType, Expr* castee)
                : Expr(Kind::ImplicitCast, startPosition, endPosition),
                  castType(castType), castee(castee) {}

        Type* castType;
        Expr* castee;

        ~ImplicitCastExpr() override {
            delete castType;
            delete castee;
        }

    };
}

#endif //GULC_IMPLICITCASTEXPR_HPP
