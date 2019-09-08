#ifndef GULC_PARAMETERDECL_HPP
#define GULC_PARAMETERDECL_HPP

#include <AST/Decl.hpp>
#include <AST/Type.hpp>
#include <AST/Expr.hpp>

namespace gulc {
    class ParameterDecl : public Decl {
    public:
        static bool classof(const Decl *decl) { return decl->getDeclKind() == Kind::Parameter; }

        ParameterDecl(std::string sourceFile, TextPosition startPosition, TextPosition endPosition, Type* type,
                      std::string name, Expr* defaultArgument = nullptr)
                : Decl(Kind::Parameter, std::move(sourceFile), startPosition, endPosition),
                  type(type), _name(std::move(name)), _defaultArgument(defaultArgument) {}

        // TODO: Support 'Modifiers' and default modifiers like 'in' and 'out'
        Type* type;
        std::string name() const { return _name; }
        const Expr* defaultArgument() const { return _defaultArgument; }
        bool hasDefaultArgument() const { return _defaultArgument != nullptr; }

        ~ParameterDecl() override {
            delete type;
            delete _defaultArgument;
        }

    private:
        const std::string _name;
        // TODO: Should this be 'ConstExpr'?
        Expr* _defaultArgument;

    };
}

#endif //GULC_PARAMETERDECL_HPP
