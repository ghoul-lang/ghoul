#ifndef GULC_RESOLVEDTYPEREFEXPR_HPP
#define GULC_RESOLVEDTYPEREFEXPR_HPP

#include <AST/Expr.hpp>
#include <AST/Type.hpp>
#include <vector>

namespace gulc {
    class ResolvedTypeRefExpr : public Expr {
    public:
        static bool classof(const Expr *expr) { return expr->getExprKind() == Kind::ResolvedTypeRef; }

        ResolvedTypeRefExpr(TextPosition startPosition, TextPosition endPosition,
                            Type* resolvedType)
                : Expr(Kind::ResolvedTypeRef, startPosition, endPosition),
                  _resolvedType(resolvedType) {}

        Type* resolvedType() const { return _resolvedType; }

        ~ResolvedTypeRefExpr() override {
            delete _resolvedType;
        }

    private:
        Type* _resolvedType;

    };
}

#endif //GULC_RESOLVEDTYPEREFEXPR_HPP
