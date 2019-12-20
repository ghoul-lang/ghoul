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

#ifndef GULC_OPERATORDECL_HPP
#define GULC_OPERATORDECL_HPP

#include <AST/Decl.hpp>
#include "FunctionDecl.hpp"

namespace gulc {
    enum class OperatorType {
        Unknown,
        Prefix,
        Infix,
        // TODO: We don't parse this right now. Only really used for '++' and '--', you can't make a custom one
        Postfix
    };

    std::string operatorTypeName(OperatorType operatorType);

    class OperatorDecl : public FunctionDecl {
    public:
        static bool classof(const Decl *decl) { return decl->getDeclKind() == Kind::Operator; }

        OperatorDecl(std::vector<Attr*> attributes, std::string sourceFile,
                     TextPosition startPosition, TextPosition endPosition,
                     OperatorType operatorType, std::string operatorName,
                     Visibility visibility, FunctionModifiers modifier, Type* resultType,
                     std::vector<ParameterDecl*> parameters, CompoundStmt* body)
                : FunctionDecl(Kind::Operator, std::move(attributes),
                               ".operator." + operatorTypeName(operatorType) + "." + operatorName,
                               std::move(sourceFile),
                               startPosition, endPosition, visibility, modifier, resultType,
                               std::move(parameters), body, {}),
                  _operatorType(operatorType), _operatorName(std::move(operatorName)) {}

        OperatorType operatorType() const { return _operatorType; }
        std::string const& operatorName() const { return _operatorName; }

        Decl* deepCopy() const override {
            std::vector<Attr*> copiedAttributes;
            std::vector<ParameterDecl*> copiedParameters;

            for (Attr* attribute : _attributes) {
                copiedAttributes.push_back(attribute->deepCopy());
            }

            for (ParameterDecl* parameter : parameters) {
                copiedParameters.push_back(llvm::dyn_cast<ParameterDecl>(parameter->deepCopy()));
            }

            auto result = new OperatorDecl(copiedAttributes, sourceFile(),
                                           startPosition(), endPosition(),
                                           _operatorType, _operatorName,
                                           visibility(), _modifier,
                                           resultType->deepCopy(),
                                           std::move(copiedParameters),
                                           llvm::dyn_cast<CompoundStmt>(_body->deepCopy()));
            result->parentNamespace = parentNamespace;
            result->parentStruct = parentStruct;
            result->labeledStmts = labeledStmts;
            return result;
        }

    private:
        OperatorType _operatorType;
        std::string _operatorName;

    };
}

#endif //GULC_OPERATORDECL_HPP
