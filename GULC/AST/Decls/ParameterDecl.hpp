#ifndef GULC_PARAMETERDECL_HPP
#define GULC_PARAMETERDECL_HPP

#include <AST/Decl.hpp>
#include <AST/Type.hpp>
#include <AST/Expr.hpp>

namespace gulc {
    class ParameterDecl : public Decl {
    public:
        static bool classof(const Decl *decl) { return decl->getDeclKind() == DeclKind::Parameter; }

        ParameterDecl(std::string sourceFile, TextPosition startPosition, TextPosition endPosition, Type* type,
                      std::string name, Expr* defaultArgument = nullptr)
                : Decl(DeclKind::Parameter, std::move(sourceFile), startPosition, endPosition),
                  _type(type), _name(std::move(name)), _defaultArgument(defaultArgument) {}

        const Type* type() const { return _type; }
        std::string name() const { return _name; }
        const Expr* defaultArgument() const { return _defaultArgument; }

        ~ParameterDecl() override {
            delete _type;
            delete _defaultArgument;
        }

    private:
        // TODO: Support 'Modifiers' and default modifiers like 'in' and 'out'
        Type* _type;
        const std::string _name;
        // TODO: Should this be 'ConstExpr'?
        Expr* _defaultArgument;

    };
}

#endif //GULC_PARAMETERDECL_HPP
