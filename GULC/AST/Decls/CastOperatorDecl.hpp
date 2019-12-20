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

#ifndef GULC_CASTOPERATORDECL_HPP
#define GULC_CASTOPERATORDECL_HPP

#include "FunctionDecl.hpp"

namespace gulc {
    enum class CastOperatorType {
        Unknown,
        Implicit,
        Explicit
    };

    std::string castOperatorTypeName(CastOperatorType castOperatorType);

    class CastOperatorDecl : public FunctionDecl {
    public:
        static bool classof(const Decl *decl) { return decl->getDeclKind() == Kind::CastOperator; }

        CastOperatorDecl(std::vector<Attr*> attributes, std::string sourceFile,
                         TextPosition startPosition, TextPosition endPosition,
                         CastOperatorType castOperatorType,
                         Visibility visibility, FunctionModifiers modifier, Type* resultType,
                         CompoundStmt* body)
                : FunctionDecl(Kind::CastOperator, std::move(attributes),
                               ".operator." + castOperatorTypeName(castOperatorType) + "." + resultType->getString(),
                               std::move(sourceFile),
                               startPosition, endPosition, visibility, modifier, resultType,
                               {}, body, {}),
                  _castOperatorType(castOperatorType) {}

        CastOperatorType castOperatorType() const { return _castOperatorType; }

        Decl* deepCopy() const override {
            std::vector<Attr*> copiedAttributes;

            for (Attr* attribute : _attributes) {
                copiedAttributes.push_back(attribute->deepCopy());
            }

            auto result = new CastOperatorDecl(copiedAttributes, sourceFile(),
                                               startPosition(), endPosition(),
                                               _castOperatorType,
                                               visibility(), _modifier,
                                               resultType->deepCopy(),
                                               static_cast<CompoundStmt*>(_body->deepCopy()));
            result->parentNamespace = parentNamespace;
            result->parentStruct = parentStruct;
            result->labeledStmts = labeledStmts;
            return result;
        }

    private:
        CastOperatorType _castOperatorType;

    };
}

#endif //GULC_CASTOPERATORDECL_HPP
