#ifndef GULC_FUNCTIONDECL_HPP
#define GULC_FUNCTIONDECL_HPP

#include <AST/Decl.hpp>
#include <AST/Type.hpp>
#include <vector>
#include <AST/Stmts/CompoundStmt.hpp>
#include "ParameterDecl.hpp"

namespace gulc {
    class FunctionDecl : public Decl {
    public:
        static bool classof(const Decl *decl) { return decl->getDeclKind() == DeclKind::Function; }

        // TODO: Support template functions. Template functions will have syntax similar to C# but support the same things as C++
        // (i.e. `int f<T>(T param1);`, `class t<int len> { int[len] array; }` where no prefix to the name defaults to 'typename')
        // (`int f<T>(T param1);` == `int f<typename T>(T param1);`)
        FunctionDecl(std::string sourceFile, TextPosition startPosition, TextPosition endPosition, Type* resultType,
                     std::string name, std::vector<ParameterDecl*> parameters, CompoundStmt* body)
                : Decl(DeclKind::Function, std::move(sourceFile), startPosition, endPosition),
                  _resultType(resultType), _name(std::move(name)), _parameters(std::move(parameters)), _body(body) {}

        const Type* resultType() const { return _resultType; }
        std::string name() const { return _name; }
        const std::vector<ParameterDecl*>& parameters() const { return _parameters; }
        const CompoundStmt* body() const { return _body; }

        ~FunctionDecl() override {
            for (ParameterDecl* parameterDecl : _parameters) {
                delete parameterDecl;
            }

            delete _resultType;
            delete _body;
        }

    private:
        Type* _resultType;
        const std::string _name;
        const std::vector<ParameterDecl*> _parameters;
        CompoundStmt* _body;

    };
}

#endif //GULC_FUNCTIONDECL_HPP
