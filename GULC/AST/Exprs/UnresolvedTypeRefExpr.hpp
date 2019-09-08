#ifndef GULC_UNRESOLVEDTYPEREFEXPR_HPP
#define GULC_UNRESOLVEDTYPEREFEXPR_HPP

#include <AST/Expr.hpp>
#include <AST/Type.hpp>
#include <vector>

namespace gulc {
    class UnresolvedTypeRefExpr : public Expr {
    public:
        static bool classof(const Expr *expr) { return expr->getExprKind() == Kind::UnresolvedTypeRef; }

        // TODO: Do we need to have template arguments?
        UnresolvedTypeRefExpr(TextPosition startPosition, TextPosition endPosition,
                              Type* unresolvedType)
                : Expr(Kind::UnresolvedTypeRef, startPosition, endPosition),
                  unresolvedType(unresolvedType) {}

        Type* unresolvedType;

        ~UnresolvedTypeRefExpr() override {
            delete unresolvedType;
        }

    };
}

#endif //GULC_UNRESOLVEDTYPEREFEXPR_HPP
