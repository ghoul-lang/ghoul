#ifndef GULC_POSTFIXOPERATOREXPR_HPP
#define GULC_POSTFIXOPERATOREXPR_HPP

#include <AST/Expr.hpp>
#include <string>

namespace gulc {
    class PostfixOperatorExpr : public Expr {
    public:
        static bool classof(const Expr *expr) { return expr->getExprKind() == ExprKind::PostfixOperator; }

        PostfixOperatorExpr(TextPosition startPosition, TextPosition endPosition,
                std::string operatorName, Expr* expr)
                : Expr(ExprKind::PostfixOperator, startPosition, endPosition),
                  _operatorName(std::move(operatorName)), _expr(expr) {}

        std::string operatorName() const { return _operatorName; }
        const Expr* expr() const { return _expr; }

        ~PostfixOperatorExpr() override {
            delete _expr;
        }

    private:
        const std::string _operatorName;
        const Expr* _expr;

    };
}

#endif //GULC_POSTFIXOPERATOREXPR_HPP
