#ifndef GULC_TEMPLATEPARAMETERDECL_HPP
#define GULC_TEMPLATEPARAMETERDECL_HPP

#include <AST/Decl.hpp>
#include <AST/Type.hpp>
#include <AST/Expr.hpp>

namespace gulc {
    class TemplateParameterDecl : public Decl {
    public:
        static bool classof(const Decl *decl) { return decl->getDeclKind() == DeclKind::TemplateParameterDecl; }

        TemplateParameterDecl(std::string sourceFile, TextPosition startPosition, TextPosition endPosition, Type* type,
                std::string name, Expr* defaultArgument = nullptr)
                : Decl(DeclKind::TemplateParameterDecl, std::move(sourceFile), startPosition, endPosition),
                  _type(type), _name(std::move(name)), _defaultArgument(defaultArgument) {}

        const Type* type() const { return _type; }
        std::string name() const { return _name; }
        const Expr* defaultArgument() const { return _defaultArgument; }
        bool hasDefaultArgument() const { return _defaultArgument != nullptr; }

        ~TemplateParameterDecl() override {
            delete _type;
            delete _defaultArgument;
        }

    private:
        Type* _type;
        const std::string _name;
        // TODO: Should this be 'ConstExpr'?
        Expr* _defaultArgument;

    };
}

#endif //GULC_TEMPLATEPARAMETERDECL_HPP
