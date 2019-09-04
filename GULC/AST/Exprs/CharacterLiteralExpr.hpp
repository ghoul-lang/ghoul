#ifndef GULC_CHARACTERLITERALEXPR_HPP
#define GULC_CHARACTERLITERALEXPR_HPP

#include <AST/Expr.hpp>

namespace gulc {
    class CharacterLiteralExpr : public Expr {
    public:
        static bool classof(const Expr *expr) { return expr->getExprKind() == ExprKind::CharacterLiteral; }

        CharacterLiteralExpr(TextPosition startPosition, TextPosition endPosition, unsigned int characterLiteral)
                : Expr(ExprKind::CharacterLiteral, startPosition, endPosition),
                  _characterValue(characterLiteral) {}

        unsigned int characterValue() const { return _characterValue; }

    private:
        unsigned int _characterValue;

    };
}

#endif //GULC_CHARACTERLITERALEXPR_HPP
