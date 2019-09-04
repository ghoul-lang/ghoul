#ifndef GULC_MEMBERACCESSCALLEXPR_HPP
#define GULC_MEMBERACCESSCALLEXPR_HPP

#include <AST/Expr.hpp>
#include "IdentifierExpr.hpp"

namespace gulc {
    class MemberAccessCallExpr : public Expr {
    public:
        static bool classof(const Expr *expr) { return expr->getExprKind() == ExprKind::MemberAccessCall; }

        MemberAccessCallExpr(TextPosition startPosition, TextPosition endPosition,
                             bool isArrowCall, Expr* objectRef, IdentifierExpr* member)
                : Expr(ExprKind::MemberAccessCall, startPosition, endPosition),
                  _isArrowCall(isArrowCall), _objectRef(objectRef), _member(member) {}

        bool isArrowCall() const { return _isArrowCall; }
        const Expr* objectRef() const { return _objectRef; }
        const IdentifierExpr* member() const { return _member; }

        ~MemberAccessCallExpr() override {
            delete _objectRef;
            delete _member;
        }

    private:
        bool _isArrowCall;
        Expr* _objectRef;
        IdentifierExpr* _member;

    };
}

#endif //GULC_MEMBERACCESSCALLEXPR_HPP
