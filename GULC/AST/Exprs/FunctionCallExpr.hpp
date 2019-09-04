#ifndef GULC_FUNCTIONCALLEXPR_HPP
#define GULC_FUNCTIONCALLEXPR_HPP

#include <AST/Expr.hpp>
#include <vector>

namespace gulc {
    class FunctionCallExpr : public Expr {
    public:
        static bool classof(const Expr *expr) { return expr->getExprKind() == ExprKind::FunctionCall; }

        FunctionCallExpr(TextPosition startPosition, TextPosition endPosition,
                         Expr* functionReference, std::vector<Expr*> arguments)
                : Expr(ExprKind::FunctionCall, startPosition, endPosition),
                  _functionReference(functionReference), _arguments(std::move(arguments)) {}

        const Expr* functionReference() const { return _functionReference; }
        const std::vector<Expr*>& arguments() const { return _arguments; }

        ~FunctionCallExpr() override {
            delete _functionReference;

            for (Expr* argument : _arguments) {
                delete argument;
            }
        }

    private:
        Expr* _functionReference;
        std::vector<Expr*> _arguments;

    };
}

#endif //GULC_FUNCTIONCALLEXPR_HPP
