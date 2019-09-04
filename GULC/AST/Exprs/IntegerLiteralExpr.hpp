#ifndef GULC_INTEGERLITERALEXPR_HPP
#define GULC_INTEGERLITERALEXPR_HPP

#include <AST/Expr.hpp>
#include <string>

namespace gulc {
    class IntegerLiteralExpr : public Expr {
    public:
        static bool classof(const Expr *expr) { return expr->getExprKind() == ExprKind::IntegerLiteral; }

        IntegerLiteralExpr(TextPosition startPosition, TextPosition endPosition,
                           int numberBase, std::string numberString)
                : Expr(ExprKind::IntegerLiteral, startPosition, endPosition),
                  _numberBase(numberBase), _numberString(std::move(numberString)) {}

        unsigned int numberBase() const { return _numberBase; }
        std::string numberString() const { return _numberString; }

    private:
        unsigned int _numberBase;
        std::string _numberString;

    };
}

#endif //GULC_INTEGERLITERALEXPR_HPP
