#ifndef GULC_PREFIXOPERATOREXPR_HPP
#define GULC_PREFIXOPERATOREXPR_HPP

#include <AST/Expr.hpp>
#include <string>

namespace gulc {
    class PrefixOperatorExpr : public Expr {
    public:
        static bool classof(const Expr *expr) { return expr->getExprKind() == Kind::PrefixOperator; }

        PrefixOperatorExpr(TextPosition startPosition, TextPosition endPosition,
                           std::string operatorName, Expr* expr)
                : Expr(Kind::PrefixOperator, startPosition, endPosition),
                  expr(expr), _operatorName(std::move(operatorName)) {}

        std::string operatorName() const { return _operatorName; }
        Expr* expr;

        ~PrefixOperatorExpr() override {
            delete expr;
        }

    private:
        std::string _operatorName;

    };
}

#endif //GULC_PREFIXOPERATOREXPR_HPP
