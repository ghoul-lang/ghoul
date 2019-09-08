#ifndef GULC_TEMPLATEPARAMETERDECL_HPP
#define GULC_TEMPLATEPARAMETERDECL_HPP

#include <AST/Decl.hpp>
#include <AST/Type.hpp>
#include <AST/Expr.hpp>

namespace gulc {
    class TemplateParameterDecl : public Decl {
    public:
        static bool classof(const Decl *decl) { return decl->getDeclKind() == Kind::TemplateParameterDecl; }

        TemplateParameterDecl(std::string sourceFile, TextPosition startPosition, TextPosition endPosition, Type* type,
                std::string name, Expr* defaultArgument = nullptr)
                : Decl(Kind::TemplateParameterDecl, std::move(sourceFile), startPosition, endPosition),
                  type(type), _name(std::move(name)), _defaultArgument(defaultArgument) {}

        Type* type;
        std::string name() const { return _name; }
        const Expr* defaultArgument() const { return _defaultArgument; }
        bool hasDefaultArgument() const { return _defaultArgument != nullptr; }

        ~TemplateParameterDecl() override {
            delete type;
            delete _defaultArgument;
        }

    private:
        const std::string _name;
        // TODO: Should this be 'ConstExpr'?
        Expr* _defaultArgument;

    };
}

#endif //GULC_TEMPLATEPARAMETERDECL_HPP
