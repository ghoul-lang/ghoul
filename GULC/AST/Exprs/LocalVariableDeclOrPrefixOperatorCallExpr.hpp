#ifndef GULC_LOCALVARIABLEDECLORPREFIXOPERATORCALLEXPR_HPP
#define GULC_LOCALVARIABLEDECLORPREFIXOPERATORCALLEXPR_HPP

#include <AST/Expr.hpp>

namespace gulc {
    class LocalVariableDeclOrPrefixOperatorCallExpr : public Expr {
    public:
        static bool classof(const Expr *expr) { return expr->getExprKind() == ExprKind::LocalVariableDeclOrPrefixOperatorCallExpr; }

        LocalVariableDeclOrPrefixOperatorCallExpr(TextPosition startPosition, TextPosition endPosition,
                                                  Expr* typeOrPrefixOperator, Expr* nameOrExpr)
                : Expr(ExprKind::LocalVariableDeclOrPrefixOperatorCallExpr, startPosition, endPosition),
                  _typeOrPrefixOperator(typeOrPrefixOperator), _nameOrExpr(nameOrExpr) {}

        const Expr* typeOrPrefixOperator() const { return _typeOrPrefixOperator; }
        const Expr* nameOrExpr() const { return _nameOrExpr; }

        ~LocalVariableDeclOrPrefixOperatorCallExpr() override {
            delete _typeOrPrefixOperator;
            delete _nameOrExpr;
        }

    private:
        Expr* _typeOrPrefixOperator;
        Expr* _nameOrExpr;

    };
}

#endif //GULC_LOCALVARIABLEDECLORPREFIXOPERATORCALLEXPR_HPP
