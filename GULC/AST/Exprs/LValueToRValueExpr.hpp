#ifndef GULC_LVALUETORVALUEEXPR_HPP
#define GULC_LVALUETORVALUEEXPR_HPP

#include <AST/Expr.hpp>

namespace gulc {
    class LValueToRValueExpr : public Expr {
    public:
        static bool classof(const Expr *expr) { return expr->getExprKind() == Kind::LValueToRValue; }

        LValueToRValueExpr(TextPosition startPosition, TextPosition endPosition, Expr* lvalue,
                           bool deletePointer = true)
                : Expr(Kind::LValueToRValue, startPosition, endPosition),
                  lvalue(lvalue), _deletePointer(deletePointer) {}

        Expr* lvalue;

        ~LValueToRValueExpr() override {
            if (_deletePointer) {
                delete lvalue;
            }
        }

    private:
        bool _deletePointer;

    };
}

#endif //GULC_LVALUETORVALUEEXPR_HPP
