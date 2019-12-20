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

#ifndef GULC_CALLOPERATORDECL_HPP
#define GULC_CALLOPERATORDECL_HPP

#include "FunctionDecl.hpp"

namespace gulc {
    /**
     * This is used to store overloads for the function call operator on structs
     *
     * Decl Syntax: `T& operator this(int param1, float param2, ...)`
     *
     * Call Syntax:
     *
     *     Function<int> func;
     *     func(12);
     *
     */
    class CallOperatorDecl : public FunctionDecl {
    public:
        static bool classof(const Decl *decl) { return decl->getDeclKind() == Kind::CallOperator; }

        CallOperatorDecl(std::vector<Attr*> attributes, std::string sourceFile,
                         TextPosition startPosition, TextPosition endPosition,
                         Visibility visibility, FunctionModifiers modifier, Type* resultType,
                         std::vector<ParameterDecl*> parameters, CompoundStmt* body)
                : FunctionDecl(Kind::CallOperator, std::move(attributes),
                               ".operator.index",
                               std::move(sourceFile),
                               startPosition, endPosition, visibility, modifier, resultType,
                               std::move(parameters), body, {}) {}

        Decl* deepCopy() const override {
            std::vector<Attr*> copiedAttributes;
            std::vector<ParameterDecl*> copiedParameters;

            for (Attr* attribute : _attributes) {
                copiedAttributes.push_back(attribute->deepCopy());
            }

            for (ParameterDecl* parameter : parameters) {
                copiedParameters.push_back(llvm::dyn_cast<ParameterDecl>(parameter->deepCopy()));
            }

            auto result = new CallOperatorDecl(copiedAttributes, sourceFile(),
                                               startPosition(), endPosition(),
                                               visibility(), _modifier,
                                               resultType->deepCopy(),
                                               std::move(copiedParameters),
                                               llvm::dyn_cast<CompoundStmt>(_body->deepCopy()));
            result->parentNamespace = parentNamespace;
            result->parentStruct = parentStruct;
            result->labeledStmts = labeledStmts;
            return result;
        }

    };
}

#endif //GULC_CALLOPERATORDECL_HPP
