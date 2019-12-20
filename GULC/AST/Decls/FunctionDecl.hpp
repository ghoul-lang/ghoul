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
#include <map>
#include <AST/Stmts/LabeledStmt.hpp>
#include "ParameterDecl.hpp"
#include "TemplateParameterDecl.hpp"

namespace gulc {
    enum class FunctionModifiers {
        None,
        Virtual,
        Override,
        Abstract,
        Static
    };

    class FunctionDecl : public Decl {
    public:
        static bool classof(const Decl *decl) {
            Kind kind = decl->getDeclKind();

            return kind == Kind::Function || kind == Kind::Operator || kind == Kind::CastOperator ||
                   kind == Kind::CallOperator || kind == Kind::IndexOperator;
        }

        FunctionDecl(std::vector<Attr*> attributes, std::string name, std::string sourceFile,
                     TextPosition startPosition, TextPosition endPosition,
                     Visibility visibility, FunctionModifiers modifier, Type* resultType,
                     std::vector<ParameterDecl*> parameters, CompoundStmt* body)
                : FunctionDecl(std::move(attributes), std::move(name), std::move(sourceFile),
                               startPosition, endPosition, visibility,
                               modifier, resultType, std::move(parameters), body, {}) {}

        FunctionDecl(std::vector<Attr*> attributes, std::string name, std::string sourceFile,
                     TextPosition startPosition, TextPosition endPosition,
                     Visibility visibility, FunctionModifiers modifier, Type* resultType,
                     std::vector<ParameterDecl*> parameters, CompoundStmt* body, std::vector<Expr*> templateArguments)
                : FunctionDecl(Kind::Function, std::move(attributes), std::move(name), std::move(sourceFile),
                               startPosition, endPosition, visibility, modifier, resultType,
                               std::move(parameters), body, std::move(templateArguments)) {}

    protected:
        // Since `OperatorDecl` extends us we allow children to set the decl kind...
        FunctionDecl(Kind kind, std::vector<Attr*> attributes, std::string name, std::string sourceFile,
                     TextPosition startPosition, TextPosition endPosition,
                     Visibility visibility, FunctionModifiers modifier, Type* resultType,
                     std::vector<ParameterDecl*> parameters, CompoundStmt* body, std::vector<Expr*> templateArguments)
                : Decl(kind, std::move(attributes), std::move(name), std::move(sourceFile),
                       startPosition, endPosition, visibility),
                  templateArguments(std::move(templateArguments)), resultType(resultType),
                  parameters(std::move(parameters)), _body(body), _isMain(false), _modifier(modifier) {
            if (this->name() == "main") {
                _isMain = true;
            }
        }

    public:

        std::vector<Expr*> templateArguments;
        Type* resultType;
        std::vector<ParameterDecl*> parameters;
        CompoundStmt* body() const { return _body; }

        bool hasParameters() const { return !parameters.empty(); }

        bool isMain() const { return _isMain; }

        FunctionModifiers modifier() const { return _modifier; }
        
        // Returns true if the function is `abstract`, `virtual`, or `override`
        bool isVirtual() const {
            return _modifier == FunctionModifiers::Abstract || _modifier == FunctionModifiers::Virtual ||
                   _modifier == FunctionModifiers::Override;
        }

        Decl* deepCopy() const override {
            std::vector<Attr*> copiedAttributes;
            std::vector<Expr*> copiedTemplateArguments;
            std::vector<ParameterDecl*> copiedParameters;

            for (Attr* attribute : _attributes) {
                copiedAttributes.push_back(attribute->deepCopy());
            }

            for (Expr* templateArgument : templateArguments) {
                copiedTemplateArguments.push_back(templateArgument->deepCopy());
            }

            for (ParameterDecl* parameter : parameters) {
                copiedParameters.push_back(static_cast<ParameterDecl*>(parameter->deepCopy()));
            }

            auto result = new FunctionDecl(copiedAttributes, name(), sourceFile(),
                                           startPosition(), endPosition(),
                                           visibility(), _modifier,
                                           resultType->deepCopy(),
                                           std::move(copiedParameters), static_cast<CompoundStmt*>(_body->deepCopy()),
                                           std::move(copiedTemplateArguments));
            result->parentNamespace = parentNamespace;
            result->parentStruct = parentStruct;
            result->labeledStmts = labeledStmts;
            return result;
        }

        // We DO NOT own this, this is ONLY to store a list of found labels for use in passes
        std::map<std::string, LabeledStmt*> labeledStmts;

        ~FunctionDecl() override {
            for (Expr* templateArgument : templateArguments) {
                delete templateArgument;
            }

            for (ParameterDecl* parameterDecl : parameters) {
                delete parameterDecl;
            }

            delete resultType;
            delete _body;
        }

    protected:
        CompoundStmt* _body;
        bool _isMain;
        FunctionModifiers _modifier;

    };
}

#endif //GULC_FUNCTIONDECL_HPP
