#ifndef GULC_PREFIXOPERATOREXPR_HPP
#define GULC_PREFIXOPERATOREXPR_HPP

#include <AST/Expr.hpp>
#include <string>

namespace gulc {
    class PrefixOperatorExpr : public Expr {
    public:
        static bool classof(const Expr *expr) { return expr->getExprKind() == ExprKind::PrefixOperator; }

        PrefixOperatorExpr(TextPosition startPosition, TextPosition endPosition,
                           std::string operatorName, Expr* expr)
                : Expr(ExprKind::PrefixOperator, startPosition, endPosition),
                  _operatorName(std::move(operatorName)), _expr(expr) {}

        std::string operatorName() const { return _operatorName; }
        const Expr* expr() const { return _expr; }

        ~PrefixOperatorExpr() override {
            delete _expr;
        }

    private:
        const std::string _operatorName;
        const Expr* _expr;

    };
}

#endif //GULC_PREFIXOPERATOREXPR_HPP
