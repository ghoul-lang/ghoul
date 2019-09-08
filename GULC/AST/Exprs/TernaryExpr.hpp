#ifndef GULC_TERNARYEXPR_HPP
#define GULC_TERNARYEXPR_HPP

#include <AST/Expr.hpp>

namespace gulc {
    class TernaryExpr : public Expr {
    public:
        static bool classof(const Expr *expr) { return expr->getExprKind() == Kind::Ternary; }

        TernaryExpr(TextPosition startPosition, TextPosition endPosition,
                    Expr* condition, Expr* trueExpr, Expr* falseExpr)
                : Expr(Kind::Ternary, startPosition, endPosition),
                  condition(condition), trueExpr(trueExpr), falseExpr(falseExpr) {}

        Expr* condition;
        Expr* trueExpr;
        Expr* falseExpr;

        ~TernaryExpr() override {
            delete condition;
            delete trueExpr;
            delete falseExpr;
        }

    private:

    };
}

#endif //GULC_TERNARYEXPR_HPP
