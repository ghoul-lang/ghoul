// Copyright (C) 2019 Michael Brandon Huddle
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as published
// by the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public License
// along with this program.  If not, see <https://www.gnu.org/licenses/>.

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

        Decl* deepCopy() const override {
            std::vector<TemplateParameterDecl*> copiedTemplateParameters;
            std::vector<ParameterDecl*> copiedParameters;

            for (TemplateParameterDecl* templateParameter : templateParameters) {
                copiedTemplateParameters.push_back(static_cast<TemplateParameterDecl*>(templateParameter->deepCopy()));
            }

            for (ParameterDecl* parameter : parameters) {
                copiedParameters.push_back(static_cast<ParameterDecl*>(parameter->deepCopy()));
            }

            return new FunctionDecl(name(), sourceFile(),
                                    startPosition(), endPosition(),
                                    resultType->deepCopy(), std::move(copiedTemplateParameters),
                                    std::move(copiedParameters), static_cast<CompoundStmt*>(_body->deepCopy()));
        }

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
