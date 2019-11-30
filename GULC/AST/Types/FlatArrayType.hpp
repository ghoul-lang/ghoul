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

#ifndef GULC_FLATARRAYTYPE_HPP
#define GULC_FLATARRAYTYPE_HPP

#include <AST/Type.hpp>
#include <AST/Expr.hpp>

namespace gulc {
    /**
     * The flat array type is just the C-style array that has to have a constant length.
     *
     * Examples:
     *     struct Example
     *     {
     *         // The type of `i` is `FlatArrayType`. This is a special scenario needed to legacy support from C and C++
     *         // NOTE: `sizeof(Example.i)` == `sizeof(indexType) * length`
     *         public int i[12];
     *         // The type of `j` is NOT `FlatArrayType`. This is a different special scenario explained elsewhere
     *         // NOTE: `sizeof(Example.j)` == `sizeof(void*)`
     *         public int[] j;
     *     }
     *
     *     int exampleFunction()
     *     {
     *         // The type of `flatArray` is `FlatArrayType`
     *         int flatArray[42];
     *         // The type of `dynamicArray` is NOT `FlatArrayType`
     *         int[] dynamicArray;
     *     }
     */
    // TODO: Should we remove this and make this apart of `GlobalVariableExpr`? Variables declared `FlatArrayType`
    //       ARE NOT passable, unless you take the address of it and make it an unsafe pointer with no compiler defined
    //       length anymore.
    class FlatArrayType : public Type {
    public:
        static bool classof(const Type *expr) { return expr->getTypeKind() == Kind::FlatArray; }

        FlatArrayType(TextPosition startPosition, TextPosition endPosition, TypeQualifier qualifier,
                      Type* indexType, Expr* length)
                : Type(Kind::FlatArray, startPosition, endPosition, qualifier),
                  indexType(indexType), length(length) {}

        Type* indexType;
        Expr* length;

        std::string getString() const override {
            return indexType->getString() + "[...]";
        }

        Type* deepCopy() const override {
            return new FlatArrayType(startPosition(), endPosition(), qualifier(),
                                     indexType->deepCopy(), length->deepCopy());
        }

        ~FlatArrayType() override {
            delete indexType;
            delete length;
        }
    };
}

#endif //GULC_FLATARRAYTYPE_HPP
