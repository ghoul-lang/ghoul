#ifndef GULC_STRINGLITERALEXPR_HPP
#define GULC_STRINGLITERALEXPR_HPP

#include <AST/Expr.hpp>
#include <string>

namespace gulc {
    class StringLiteralExpr : public Expr {
    public:
        static bool classof(const Expr *expr) { return expr->getExprKind() == Kind::StringLiteral; }

        StringLiteralExpr(TextPosition startPosition, TextPosition endPosition, std::string stringValue)
                : Expr(Kind::StringLiteral, startPosition, endPosition),
                  _stringValue(std::move(stringValue)) {}

        std::string stringValue() const { return _stringValue; }

    private:
        std::string _stringValue;

    };
}

#endif //GULC_STRINGLITERALEXPR_HPP
