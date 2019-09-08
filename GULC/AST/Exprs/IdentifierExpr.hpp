#ifndef GULC_IDENTIFIEREXPR_HPP
#define GULC_IDENTIFIEREXPR_HPP

#include <AST/Expr.hpp>
#include <string>
#include <vector>

namespace gulc {
    class IdentifierExpr : public Expr {
    public:
        static bool classof(const Expr *expr) { return expr->getExprKind() == Kind::Identifier; }

        IdentifierExpr(TextPosition startPosition, TextPosition endPosition, std::string name,
                       std::vector<Expr*> templateArguments)
                : Expr(Kind::Identifier, startPosition, endPosition), 
			      templateArguments(std::move(templateArguments)), _name(std::move(name)) {}

        std::string name() const { return _name; }
        std::vector<Expr*> templateArguments;
        bool hasTemplateArguments() const { return !templateArguments.empty(); }

        ~IdentifierExpr() override {
            for (Expr* templateArgument : templateArguments) {
                delete templateArgument;
            }
        }
                  
    private:
        std::string _name;

    };
}

#endif //GULC_IDENTIFIEREXPR_HPP
