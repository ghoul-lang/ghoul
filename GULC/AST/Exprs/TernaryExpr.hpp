#ifndef GULC_TERNARYEXPR_HPP
#define GULC_TERNARYEXPR_HPP

#include <AST/Expr.hpp>

namespace gulc {
    class TernaryExpr : public Expr {
    public:
        static bool classof(const Expr *expr) { return expr->getExprKind() == ExprKind::Ternary; }

        TernaryExpr(TextPosition startPosition, TextPosition endPosition,
                    Expr* condition, Expr* trueExpr, Expr* falseExpr)
                : Expr(ExprKind::Ternary, startPosition, endPosition),
                  _condition(condition), _trueExpr(trueExpr), _falseExpr(falseExpr) {}

        const Expr* condition() const { return _condition; }
        const Expr* trueExpr() const { return _trueExpr; }
        const Expr* falseExpr() const { return _falseExpr; }

        ~TernaryExpr() override {
            delete _condition;
            delete _trueExpr;
            delete _falseExpr;
        }

    private:
        Expr* _condition;
        Expr* _trueExpr;
        Expr* _falseExpr;

    };
}

#endif //GULC_TERNARYEXPR_HPP
