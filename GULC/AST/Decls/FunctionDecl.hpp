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
        static bool classof(const Decl *decl) { return decl->getDeclKind() == Kind::Function; }

        FunctionDecl(std::string name, std::string sourceFile, TextPosition startPosition, TextPosition endPosition,
                     Type* resultType, std::vector<TemplateParameterDecl*> templateParameters,
                     std::vector<ParameterDecl*> parameters, CompoundStmt* body)
                : Decl(Kind::Function, std::move(name), std::move(sourceFile), startPosition, endPosition),
                  resultType(resultType), templateParameters(std::move(templateParameters)),
                  parameters(std::move(parameters)), _body(body) {}

        Type* resultType;
        std::vector<TemplateParameterDecl*> templateParameters;
        std::vector<ParameterDecl*> parameters;
        CompoundStmt* body() const { return _body; }

        bool hasTemplateParameters() const { return !templateParameters.empty(); }
        bool hasParameters() const { return !parameters.empty(); }

        ~FunctionDecl() override {
            for (ParameterDecl* parameterDecl : parameters) {
                delete parameterDecl;
            }

            for (TemplateParameterDecl* templateParameterDecl : templateParameters) {
                delete templateParameterDecl;
            }

            delete resultType;
            delete _body;
        }

    private:
        CompoundStmt* _body;

    };
}

#endif //GULC_FUNCTIONDECL_HPP
