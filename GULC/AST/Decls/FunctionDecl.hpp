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

        FunctionDecl(std::string sourceFile, TextPosition startPosition, TextPosition endPosition, Type* resultType,
                     std::string name, std::vector<TemplateParameterDecl*> templateParameters,
                     std::vector<ParameterDecl*> parameters, CompoundStmt* body)
                : Decl(Kind::Function, std::move(sourceFile), startPosition, endPosition),
                  resultType(resultType), templateParameters(std::move(templateParameters)), _name(std::move(name)),
                  _parameters(std::move(parameters)), _body(body) {}

        Type* resultType;
        std::string name() const { return _name; }
        std::vector<TemplateParameterDecl*> templateParameters;
        std::vector<ParameterDecl*>& parameters() { return _parameters; }
        std::vector<ParameterDecl*> parameters() const { return _parameters; }
        CompoundStmt* body() const { return _body; }

        bool hasTemplateArguments() const { return !templateParameters.empty(); }

        ~FunctionDecl() override {
            for (ParameterDecl* parameterDecl : _parameters) {
                delete parameterDecl;
            }

            for (TemplateParameterDecl* templateParameterDecl : templateParameters) {
                delete templateParameterDecl;
            }

            delete resultType;
            delete _body;
        }

    private:
        const std::string _name;
        std::vector<ParameterDecl*> _parameters;
        CompoundStmt* _body;

    };
}

#endif //GULC_FUNCTIONDECL_HPP
