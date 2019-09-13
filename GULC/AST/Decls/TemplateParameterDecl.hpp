#ifndef GULC_TEMPLATEPARAMETERDECL_HPP
#define GULC_TEMPLATEPARAMETERDECL_HPP

#include <AST/Decl.hpp>
#include <AST/Type.hpp>
#include <AST/Expr.hpp>

namespace gulc {
    class TemplateParameterDecl : public Decl {
    public:
        static bool classof(const Decl *decl) { return decl->getDeclKind() == Kind::TemplateParameterDecl; }

        TemplateParameterDecl(std::string name, std::string sourceFile, TextPosition startPosition, TextPosition endPosition,
                              Type* type, Expr* defaultArgument = nullptr)
                : Decl(Kind::TemplateParameterDecl, std::move(name), std::move(sourceFile), startPosition, endPosition),
                  type(type), _defaultArgument(defaultArgument) {}

        Type* type;
        const Expr* defaultArgument() const { return _defaultArgument; }
        bool hasDefaultArgument() const { return _defaultArgument != nullptr; }

        ~TemplateParameterDecl() override {
            delete type;
            delete _defaultArgument;
        }

    private:
        // TODO: Should this be 'ConstExpr'?
        Expr* _defaultArgument;

    };
}

#endif //GULC_TEMPLATEPARAMETERDECL_HPP
