#ifndef GULC_POTENTIALEXPLICITCASTEXPR_HPP
#define GULC_POTENTIALEXPLICITCASTEXPR_HPP

#include <AST/Expr.hpp>

namespace gulc {
    class PotentialExplicitCastExpr : public Expr {
    public:
        static bool classof(const Expr *expr) { return expr->getExprKind() == Kind::PotentialExplicitCast; }

        PotentialExplicitCastExpr(TextPosition startPosition, TextPosition endPosition,
                                  Expr* castType, Expr* castee)
                : Expr(Kind::PotentialExplicitCast, startPosition, endPosition),
                  castType(castType), castee(castee) {}

        Expr* castType;
        Expr* castee;

        ~PotentialExplicitCastExpr() override {
            delete castType;
            delete castee;
        }

    private:

    };
}

#endif //GULC_POTENTIALEXPLICITCASTEXPR_HPP
