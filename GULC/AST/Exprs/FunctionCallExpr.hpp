#ifndef GULC_FUNCTIONCALLEXPR_HPP
#define GULC_FUNCTIONCALLEXPR_HPP

#include <AST/Expr.hpp>
#include <vector>

namespace gulc {
    class FunctionCallExpr : public Expr {
    public:
        static bool classof(const Expr *expr) { return expr->getExprKind() == Kind::FunctionCall; }

        FunctionCallExpr(TextPosition startPosition, TextPosition endPosition,
                         Expr* functionReference, std::vector<Expr*> arguments)
                : Expr(Kind::FunctionCall, startPosition, endPosition),
                  functionReference(functionReference), arguments(std::move(arguments)) {}

        Expr* functionReference;
        std::vector<Expr*> arguments;
        bool hasArguments() const { return !arguments.empty(); }

        ~FunctionCallExpr() override {
            delete functionReference;

            for (Expr* argument : arguments) {
                delete argument;
            }
        }

    };
}

#endif //GULC_FUNCTIONCALLEXPR_HPP
