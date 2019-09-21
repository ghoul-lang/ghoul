#ifndef GULC_LOCALVARIABLEDECLEXPR_HPP
#define GULC_LOCALVARIABLEDECLEXPR_HPP

#include <AST/Expr.hpp>
#include <string>

namespace gulc {
    class LocalVariableDeclExpr : public Expr {
    public:
        static bool classof(const Expr *expr) { return expr->getExprKind() == Kind::LocalVariableDecl; }

        LocalVariableDeclExpr(TextPosition startPosition, TextPosition endPosition,
                              Expr* type, std::string name)
                : Expr(Kind::LocalVariableDecl, startPosition, endPosition),
                  type(type), _name(std::move(name)) {}

        Expr* type;
        std::string name() const { return _name; }

        ~LocalVariableDeclExpr() override {
            delete type;
        }

    private:
        std::string _name;

    };
}

#endif //GULC_LOCALVARIABLEDECLEXPR_HPP
