#ifndef GULC_PARENEXPR_HPP
#define GULC_PARENEXPR_HPP

#include <AST/Expr.hpp>

namespace gulc {
    class ParenExpr : public Expr {
    public:
        static bool classof(const Expr *expr) { return expr->getExprKind() == Kind::Paren; }

        ParenExpr(TextPosition startPosition, TextPosition endPosition, Expr* containedExpr)
                : Expr(Kind::Paren, startPosition, endPosition),
                  containedExpr(containedExpr) {}

        Expr* containedExpr;

        ~ParenExpr() override {
            delete containedExpr;
        }

    private:

    };
}

#endif //GULC_PARENEXPR_HPP
