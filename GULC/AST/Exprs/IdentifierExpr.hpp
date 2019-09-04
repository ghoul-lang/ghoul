#ifndef GULC_IDENTIFIEREXPR_HPP
#define GULC_IDENTIFIEREXPR_HPP

#include <AST/Expr.hpp>
#include <string>
#include <vector>

namespace gulc {
    class IdentifierExpr : public Expr {
    public:
        static bool classof(const Expr *expr) { return expr->getExprKind() == ExprKind::Identifier; }

        IdentifierExpr(TextPosition startPosition, TextPosition endPosition, std::string name,
                       std::vector<Expr*> templateArguments)
                : Expr(ExprKind::Identifier, startPosition, endPosition), _name(std::move(name)),
                  _templateArguments(std::move(templateArguments)) {}

        std::string name() const { return _name; }
        const std::vector<Expr*>& templateArguments() const { return _templateArguments; }
                  
    private:
        std::string _name;
        std::vector<Expr*> _templateArguments;

    };
}

#endif //GULC_IDENTIFIEREXPR_HPP
