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
                  functionReference(functionReference), _arguments(std::move(arguments)) {}

        Expr* functionReference;
        std::vector<Expr*>& arguments() { return _arguments; }
        const std::vector<Expr*>& arguments() const { return _arguments; }
        bool hasArguments() const { return !_arguments.empty(); }

        ~FunctionCallExpr() override {
            delete functionReference;

            for (Expr* argument : _arguments) {
                delete argument;
            }
        }

    private:
        std::vector<Expr*> _arguments;

    };
}

#endif //GULC_FUNCTIONCALLEXPR_HPP
