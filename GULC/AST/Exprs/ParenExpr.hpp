#ifndef GULC_PARENEXPR_HPP
#define GULC_PARENEXPR_HPP

#include <AST/Expr.hpp>

namespace gulc {
    class ParenExpr : public Expr {
    public:
        static bool classof(const Expr *expr) { return expr->getExprKind() == ExprKind::Paren; }

        ParenExpr(TextPosition startPosition, TextPosition endPosition, Expr* containedExpr)
                : Expr(ExprKind::Paren, startPosition, endPosition),
                  _containedExpr(containedExpr) {}

        Expr* containedExpr() const { return _containedExpr; }

        ~ParenExpr() override {
            delete _containedExpr;
        }

    private:
        Expr* _containedExpr;

    };
}

#endif //GULC_PARENEXPR_HPP
