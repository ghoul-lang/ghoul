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
                  _type(type), _name(std::move(name)) {}

        Expr* type() const { return _type; }
        std::string name() const { return _name; }

        ~LocalVariableDeclExpr() override {
            delete _type;
        }

    private:
        Expr* _type;
        std::string _name;

    };
}

#endif //GULC_LOCALVARIABLEDECLEXPR_HPP
