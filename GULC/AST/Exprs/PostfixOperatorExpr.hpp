#ifndef GULC_POSTFIXOPERATOREXPR_HPP
#define GULC_POSTFIXOPERATOREXPR_HPP

#include <AST/Expr.hpp>
#include <string>

namespace gulc {
    class PostfixOperatorExpr : public Expr {
    public:
        static bool classof(const Expr *expr) { return expr->getExprKind() == Kind::PostfixOperator; }

        PostfixOperatorExpr(TextPosition startPosition, TextPosition endPosition,
                std::string operatorName, Expr* expr)
                : Expr(Kind::PostfixOperator, startPosition, endPosition), 
			      expr(expr), _operatorName(std::move(operatorName)) {}

        std::string operatorName() const { return _operatorName; }
        Expr* expr;

        ~PostfixOperatorExpr() override {
            delete expr;
        }

    private:
        std::string _operatorName;

    };
}

#endif //GULC_POSTFIXOPERATOREXPR_HPP
