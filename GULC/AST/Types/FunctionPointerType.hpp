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

#ifndef GULC_FUNCTIONPOINTERTYPE_HPP
#define GULC_FUNCTIONPOINTERTYPE_HPP

#include <AST/Type.hpp>
#include <vector>

namespace gulc {
    class FunctionPointerType : public Type {
    public:
        static bool classof(const Type *expr) { return expr->getTypeKind() == Kind::FunctionPointer; }

        FunctionPointerType(TextPosition startPosition, TextPosition endPosition, TypeQualifier qualifier,
                            Type* resultType, std::vector<Type*> paramTypes)
                : Type(Kind::FunctionPointer, startPosition, endPosition, qualifier),
                  resultType(resultType), paramTypes(std::move(paramTypes)) {}

        Type* resultType;
        std::vector<Type*> paramTypes;

        std::string getString() const override {
            std::string result = resultType->getString();
            result += " fn(";

            for (const Type* paramType : paramTypes) {
                result += paramType->getString();
            }

            return result + ")";
        }

        Type* deepCopy() const override {
            std::vector<Type*> copiedParamTypes;

            for (Type* paramType : paramTypes) {
                copiedParamTypes.push_back(paramType->deepCopy());
            }

            return new FunctionPointerType(startPosition(), endPosition(), qualifier(),
                                           resultType->deepCopy(), std::move(copiedParamTypes));
        }

        ~FunctionPointerType() override {
            delete resultType;

            for (Type* paramType : paramTypes) {
                delete paramType;
            }
        }

    };
}

#endif //GULC_FUNCTIONPOINTERTYPE_HPP
