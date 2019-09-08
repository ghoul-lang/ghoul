#ifndef GULC_MEMBERACCESSCALLEXPR_HPP
#define GULC_MEMBERACCESSCALLEXPR_HPP

#include <AST/Expr.hpp>
#include "IdentifierExpr.hpp"

namespace gulc {
    class MemberAccessCallExpr : public Expr {
    public:
        static bool classof(const Expr *expr) { return expr->getExprKind() == Kind::MemberAccessCall; }

        MemberAccessCallExpr(TextPosition startPosition, TextPosition endPosition,
                             bool isArrowCall, Expr* objectRef, IdentifierExpr* member)
                : Expr(Kind::MemberAccessCall, startPosition, endPosition),
                  objectRef(objectRef), member(member), _isArrowCall(isArrowCall) {}

        bool isArrowCall() const { return _isArrowCall; }
        Expr* objectRef;
        IdentifierExpr* member;

        ~MemberAccessCallExpr() override {
            delete objectRef;
            delete member;
        }

    private:
        bool _isArrowCall;

    };
}

#endif //GULC_MEMBERACCESSCALLEXPR_HPP
