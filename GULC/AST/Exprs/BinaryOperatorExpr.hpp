#ifndef GULC_BINARYOPERATOREXPR_HPP
#define GULC_BINARYOPERATOREXPR_HPP

#include <AST/Expr.hpp>
#include <string>

namespace gulc {
    class BinaryOperatorExpr : public Expr {
    public:
        static bool classof(const Expr *expr) { return expr->getExprKind() == ExprKind::BinaryOperator; }

        BinaryOperatorExpr(TextPosition startPosition, TextPosition endPosition,
                           std::string operatorName, Expr* leftValue, Expr* rightValue)
                : Expr(ExprKind::BinaryOperator, startPosition, endPosition),
                  _operatorName(std::move(operatorName)), _leftValue(leftValue), _rightValue(rightValue) {}

        std::string operatorName() const { return _operatorName; }
        const Expr* leftValue() const { return _leftValue; }
        const Expr* rightValue() const { return _rightValue; }

        ~BinaryOperatorExpr() override {
            delete _leftValue;
            delete _rightValue;
        }

    private:
        const std::string _operatorName;
        const Expr* _leftValue;
        const Expr* _rightValue;

    };
}

#endif //GULC_BINARYOPERATOREXPR_HPP
