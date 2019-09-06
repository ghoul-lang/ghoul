#ifndef GULC_FUNCTIONDECL_HPP
#define GULC_FUNCTIONDECL_HPP

#include <AST/Decl.hpp>
#include <AST/Type.hpp>
#include <vector>
#include <AST/Stmts/CompoundStmt.hpp>
#include "ParameterDecl.hpp"
#include "TemplateParameterDecl.hpp"

namespace gulc {
    class FunctionDecl : public Decl {
    public:
        static bool classof(const Decl *decl) { return decl->getDeclKind() == DeclKind::Function; }

        FunctionDecl(std::string sourceFile, TextPosition startPosition, TextPosition endPosition, Type* resultType,
                     std::string name, std::vector<TemplateParameterDecl*> templateParameters,
                     std::vector<ParameterDecl*> parameters, CompoundStmt* body)
                : Decl(DeclKind::Function, std::move(sourceFile), startPosition, endPosition),
                  _resultType(resultType), _name(std::move(name)), _templateParameters(std::move(templateParameters)),
                  _parameters(std::move(parameters)), _body(body) {}

        const Type* resultType() const { return _resultType; }
        std::string name() const { return _name; }
        const std::vector<TemplateParameterDecl*>& templateParameters() const { return _templateParameters; }
        const std::vector<ParameterDecl*>& parameters() const { return _parameters; }
        const CompoundStmt* body() const { return _body; }

        ~FunctionDecl() override {
            for (ParameterDecl* parameterDecl : _parameters) {
                delete parameterDecl;
            }

            for (TemplateParameterDecl* templateParameterDecl : _templateParameters) {
                delete templateParameterDecl;
            }

            delete _resultType;
            delete _body;
        }

    private:
        Type* _resultType;
        const std::string _name;
        const std::vector<TemplateParameterDecl*> _templateParameters;
        const std::vector<ParameterDecl*> _parameters;
        CompoundStmt* _body;

    };
}

#endif //GULC_FUNCTIONDECL_HPP
