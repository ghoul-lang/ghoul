#ifndef GULC_FLOATLITERALEXPR_HPP
#define GULC_FLOATLITERALEXPR_HPP

#include <AST/Expr.hpp>
#include <string>

namespace gulc {
    class FloatLiteralExpr : public Expr {
    public:
        static bool classof(const Expr *expr) { return expr->getExprKind() == Kind::FloatLiteral; }

        FloatLiteralExpr(TextPosition startPosition, TextPosition endPosition, std::string numberValue)
                : Expr(Kind::FloatLiteral, startPosition, endPosition),
                  _numberValue(std::move(numberValue)) {}

        std::string numberValue() const { return _numberValue; }

    private:
        std::string _numberValue;

    };
}

#endif //GULC_FLOATLITERALEXPR_HPP
