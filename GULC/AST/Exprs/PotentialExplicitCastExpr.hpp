#ifndef GULC_POTENTIALEXPLICITCASTEXPR_HPP
#define GULC_POTENTIALEXPLICITCASTEXPR_HPP

#include <AST/Expr.hpp>

namespace gulc {
    class PotentialExplicitCastExpr : public Expr {
    public:
        static bool classof(const Expr *expr) { return expr->getExprKind() == ExprKind::PotentialExplicitCast; }

        PotentialExplicitCastExpr(TextPosition startPosition, TextPosition endPosition,
                                  Expr* castType, Expr* castee)
                : Expr(ExprKind::PotentialExplicitCast, startPosition, endPosition),
                  _castType(castType), _castee(castee) {}

        const Expr* castType() const { return _castType; }
        const Expr* castee() const { return _castee; }

        ~PotentialExplicitCastExpr() override {
            delete _castType;
            delete _castee;
        }

    private:
        Expr* _castType;
        Expr* _castee;

    };
}

#endif //GULC_POTENTIALEXPLICITCASTEXPR_HPP
