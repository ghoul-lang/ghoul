#ifndef GULC_INDEXERCALLEXPR_HPP
#define GULC_INDEXERCALLEXPR_HPP

#include <AST/Expr.hpp>
#include <vector>

namespace gulc {
    class IndexerCallExpr : public Expr {
    public:
        static bool classof(const Expr *expr) { return expr->getExprKind() == Kind::IndexerCall; }

        IndexerCallExpr(TextPosition startPosition, TextPosition endPosition,
                Expr* indexerReference, std::vector<Expr*> arguments)
                : Expr(Kind::IndexerCall, startPosition, endPosition),
                  indexerReference(indexerReference), _arguments(std::move(arguments)) {}

        Expr* indexerReference;
        std::vector<Expr*>& arguments() { return _arguments; }
        const std::vector<Expr*>& arguments() const { return _arguments; }
        bool hasArguments() const { return !_arguments.empty(); }

        ~IndexerCallExpr() override {
            delete indexerReference;

            for (Expr* argument : _arguments) {
                delete argument;
            }
        }

    private:
        std::vector<Expr*> _arguments;

    };
}

#endif //GULC_INDEXERCALLEXPR_HPP
