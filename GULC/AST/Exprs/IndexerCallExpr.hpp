#ifndef GULC_INDEXERCALLEXPR_HPP
#define GULC_INDEXERCALLEXPR_HPP

#include <AST/Expr.hpp>
#include <vector>

namespace gulc {
    class IndexerCallExpr : public Expr {
    public:
        static bool classof(const Expr *expr) { return expr->getExprKind() == ExprKind::IndexerCall; }

        IndexerCallExpr(TextPosition startPosition, TextPosition endPosition,
                Expr* indexerReference, std::vector<Expr*> arguments)
                : Expr(ExprKind::IndexerCall, startPosition, endPosition),
                  _indexerReference(indexerReference), _arguments(std::move(arguments)) {}

        const Expr* indexerReference() const { return _indexerReference; }
        const std::vector<Expr*>& arguments() const { return _arguments; }
        bool hasArguments() const { return !_arguments.empty(); }

        ~IndexerCallExpr() override {
            delete _indexerReference;

            for (Expr* argument : _arguments) {
                delete argument;
            }
        }

    private:
        Expr* _indexerReference;
        std::vector<Expr*> _arguments;

    };
}

#endif //GULC_INDEXERCALLEXPR_HPP
