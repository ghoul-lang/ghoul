#ifndef GULC_LOCALVARIABLEDECLORPREFIXOPERATORCALLEXPR_HPP
#define GULC_LOCALVARIABLEDECLORPREFIXOPERATORCALLEXPR_HPP

#include <AST/Expr.hpp>

namespace gulc {
    class LocalVariableDeclOrPrefixOperatorCallExpr : public Expr {
    public:
        static bool classof(const Expr *expr) { return expr->getExprKind() == Kind::LocalVariableDeclOrPrefixOperatorCallExpr; }

        LocalVariableDeclOrPrefixOperatorCallExpr(TextPosition startPosition, TextPosition endPosition,
                                                  Expr* typeOrPrefixOperator, Expr* nameOrExpr)
                : Expr(Kind::LocalVariableDeclOrPrefixOperatorCallExpr, startPosition, endPosition),
                  typeOrPrefixOperator(typeOrPrefixOperator), nameOrExpr(nameOrExpr) {}

        Expr* typeOrPrefixOperator;
        Expr* nameOrExpr;

        ~LocalVariableDeclOrPrefixOperatorCallExpr() override {
            delete typeOrPrefixOperator;
            delete nameOrExpr;
        }

    private:

    };
}

#endif //GULC_LOCALVARIABLEDECLORPREFIXOPERATORCALLEXPR_HPP
