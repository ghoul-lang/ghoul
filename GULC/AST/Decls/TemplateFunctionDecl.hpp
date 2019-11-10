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

#ifndef GULC_TEMPLATEFUNCTIONDECL_HPP
#define GULC_TEMPLATEFUNCTIONDECL_HPP

#include <AST/Decl.hpp>
#include <AST/Type.hpp>
#include <vector>
#include <AST/Stmts/CompoundStmt.hpp>
#include <AST/Types/TemplateTypenameType.hpp>
#include <AST/Exprs/ResolvedTypeRefExpr.hpp>
#include <AST/Types/BuiltInType.hpp>
#include <AST/Exprs/IntegerLiteralExpr.hpp>
#include <AST/Exprs/FloatLiteralExpr.hpp>
#include <AST/TypeComparer.hpp>
#include "TemplateParameterDecl.hpp"
#include "ParameterDecl.hpp"
#include "FunctionDecl.hpp"

namespace gulc {
    class TemplateFunctionDecl : public Decl {
    public:
        static bool classof(const Decl *decl) { return decl->getDeclKind() == Kind::TemplateFunction; }

        TemplateFunctionDecl(std::string name, std::string sourceFile, TextPosition startPosition, TextPosition endPosition,
                             Visibility visibility, Type* resultType,
                             std::vector<TemplateParameterDecl*> templateParameters,
                             std::vector<ParameterDecl*> parameters, CompoundStmt* body)
                : TemplateFunctionDecl(std::move(name), std::move(sourceFile), startPosition, endPosition, visibility,
                                       resultType, std::move(templateParameters), std::move(parameters), body, {}) {}

        TemplateFunctionDecl(std::string name, std::string sourceFile, TextPosition startPosition, TextPosition endPosition,
                             Visibility visibility, Type* resultType,
                             std::vector<TemplateParameterDecl*> templateParameters,
                             std::vector<ParameterDecl*> parameters, CompoundStmt* body,
                             std::vector<FunctionDecl*> implementedFunctions)
                : Decl(Kind::TemplateFunction, std::move(name), std::move(sourceFile), startPosition, endPosition,
                       visibility),
                  resultType(resultType), templateParameters(std::move(templateParameters)),
                  parameters(std::move(parameters)), _body(body),
                  _implementedFunctions(std::move(implementedFunctions)) {}

        Type* resultType;
        std::vector<TemplateParameterDecl*> templateParameters;
        std::vector<ParameterDecl*> parameters;
        CompoundStmt* body() const { return _body; }
        std::vector<FunctionDecl*>& implementedFunctions() { return _implementedFunctions; }
        const std::vector<FunctionDecl*>& implementedFunctions() const { return _implementedFunctions; }

        bool hasTemplateParameters() const { return !templateParameters.empty(); }
        bool hasParameters() const { return !parameters.empty(); }

        FunctionDecl* getOrCreateFunction(std::vector<Expr*> templateArguments, bool* isNewFunction) {
            *isNewFunction = false;

            std::vector<Expr*> resultTemplateArguments;
            resultTemplateArguments.reserve(templateParameters.size());

            if (templateArguments.size() > templateParameters.size()) {
                return nullptr;
            }

            for (std::size_t i = 0; i < templateParameters.size(); ++i) {
                if (i >= templateArguments.size()) {
                    // If `i` is greater than the size of `templateArguments` then `templateParameters[i]` MUST have a default value...
                    if (!templateParameters[i]->hasDefaultArgument()) {
                        return nullptr;
                    }

                    resultTemplateArguments.push_back(templateParameters[i]->defaultArgument()->deepCopy());
                } else {
                    if (llvm::isa<TemplateTypenameType>(templateParameters[i]->type)) {
                        if (!llvm::isa<ResolvedTypeRefExpr>(templateArguments[i])) {
                            // If the template parameter is a typename then the argument MUST be a type
                            return nullptr;
                        }
                    } else {
                        if (!llvm::isa<BuiltInType>(templateParameters[i]->type)) {
                            // We currently only support built in types
                            // TODO: Support any `constexpr` types...
                            return nullptr;
                        }

                        if (!(llvm::isa<IntegerLiteralExpr>(templateArguments[i]) ||
                              llvm::isa<FloatLiteralExpr>(templateArguments[i]))) {
                            // We currently only support integer and float literals...
                            return nullptr;
                        }
                    }

                    // TODO: Check that the types match for `templateParameters[i]` and `templateArguments[i]`
                    resultTemplateArguments.push_back(templateArguments[i]->deepCopy());
                }
            }

            for (FunctionDecl* checkFunction : _implementedFunctions) {
                for (std::size_t i = 0; i < resultTemplateArguments.size(); ++i) {
                    // We will check if `resultTemplateArguments` match any implementations...
                    if (llvm::isa<ResolvedTypeRefExpr>(resultTemplateArguments[i])) {
                        if (!llvm::isa<ResolvedTypeRefExpr>(checkFunction->templateArguments[i])) {
                            goto checkNextFunction;
                        }

                        auto resultTemplateArgument = llvm::dyn_cast<ResolvedTypeRefExpr>(resultTemplateArguments[i]);
                        auto checkTemplateArgument = llvm::dyn_cast<ResolvedTypeRefExpr>(checkFunction->templateArguments[i]);

                        // If the resolved types are the same then we can just return the `checkFunction...
                        if (!TypeComparer::getTypesAreSame(resultTemplateArgument->resolvedType,
                                                           checkTemplateArgument->resolvedType)) {
                            goto checkNextFunction;
                        }
                    } else {
                        if (llvm::isa<IntegerLiteralExpr>(resultTemplateArguments[i])) {
                            if (!llvm::isa<IntegerLiteralExpr>(checkFunction->templateArguments[i])) {
                                // Types must match EXACTLY
                                goto checkNextFunction;
                            }

                            auto resultIntegerLiteral = llvm::dyn_cast<IntegerLiteralExpr>(resultTemplateArguments[i]);
                            auto checkIntegerLiteral = llvm::dyn_cast<IntegerLiteralExpr>(checkFunction->templateArguments[i]);

                            if (resultIntegerLiteral->numberString != checkIntegerLiteral->numberString ||
                                resultIntegerLiteral->numberBase() != checkIntegerLiteral->numberBase()) {
                                goto checkNextFunction;
                            }
                        } else if (llvm::isa<FloatLiteralExpr>(resultTemplateArguments[i])) {
                            if (!llvm::isa<FloatLiteralExpr>(checkFunction->templateArguments[i])) {
                                goto checkNextFunction;
                            }

                            auto resultFloatLiteral = llvm::dyn_cast<FloatLiteralExpr>(resultTemplateArguments[i]);
                            auto checkFloatLiteral = llvm::dyn_cast<FloatLiteralExpr>(checkFunction->templateArguments[i]);

                            if (resultFloatLiteral->numberValue() != checkFloatLiteral->numberValue()) {
                                goto checkNextFunction;
                            }
                        } else {
                            // TODO: Should we add a warning or error here?
                            goto checkNextFunction;
                        }
                    }
                }

                // If we reach this point it means everything matched between the `checkFunction->templateArguments` and `resultTemplateArguments`
                return checkFunction;

            checkNextFunction:
                continue;
            }

            std::vector<ParameterDecl*> copiedParameters;

            for (ParameterDecl* parameter : parameters) {
                copiedParameters.push_back(static_cast<ParameterDecl*>(parameter->deepCopy()));
            }

            *isNewFunction = true;

            auto newFunction = new FunctionDecl(name(), sourceFile(),
                                                startPosition(), endPosition(),
                                                visibility(),
                                                resultType->deepCopy(), copiedParameters,
                                                static_cast<CompoundStmt*>(_body->deepCopy()),
                                                resultTemplateArguments);

            _implementedFunctions.push_back(newFunction);

            return newFunction;
        }

        Decl* deepCopy() const override {
            std::vector<TemplateParameterDecl*> copiedTemplateParameters;
            std::vector<ParameterDecl*> copiedParameters;
            std::vector<FunctionDecl*> copiedImplementedFunctions;

            for (TemplateParameterDecl* templateParameter : templateParameters) {
                copiedTemplateParameters.push_back(static_cast<TemplateParameterDecl*>(templateParameter->deepCopy()));
            }

            for (ParameterDecl* parameter : parameters) {
                copiedParameters.push_back(static_cast<ParameterDecl*>(parameter->deepCopy()));
            }

            auto result = new FunctionDecl(name(), sourceFile(),
                                           startPosition(), endPosition(),
                                           visibility(),
                                           resultType->deepCopy(),
                                           std::move(copiedParameters), static_cast<CompoundStmt*>(_body->deepCopy()));
            result->parentNamespace = parentNamespace;
            result->parentStruct = parentStruct;
            return result;
        }

        ~TemplateFunctionDecl() override {
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
        std::vector<FunctionDecl*> _implementedFunctions;

    };
}

#endif //GULC_TEMPLATEFUNCTIONDECL_HPP
