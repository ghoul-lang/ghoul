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

#ifndef GULC_ASSIGNMENTBINARYOPERATOREXPR_HPP
#define GULC_ASSIGNMENTBINARYOPERATOREXPR_HPP

#include <AST/Expr.hpp>
#include <AST/Decls/OperatorDecl.hpp>

namespace gulc {
    /**
     * The assignment binary operator expression is an expression that tries to make operators such as `+=` et. al.
     * easier to handle. Due to GUL or U not supporting operator overloading on any assignment operators we need to
     * split the `+=` into `left = left + right`. Due to `left` potentially being a function call that we can ONLY
     * call once we have to have this class to handle the representation of `+=` in a way where the `+` is easily
     * overridden and won't be prone to bugs (e.g. having a `RefResultExpr` for the left value that might not be
     * notified of its referenced expression being optimized out)
     *
     * This class can also help to make detecting errors such as "int i += 12;" easier while still supporting what
     * we need here
     */
    class AssignmentBinaryOperatorExpr : public Expr {
    public:
        static bool classof(const Expr *expr) { return expr->getExprKind() == Kind::AssignmentBinaryOperator; }

        AssignmentBinaryOperatorExpr(TextPosition startPosition, TextPosition endPosition,
                                     Expr* leftValue, Expr* rightValue)
                : Expr(Kind::AssignmentBinaryOperator, startPosition, endPosition),
                  leftValue(leftValue), rightValue(rightValue), _hasNestedOperator(false), _nestedOperator(""),
                  nestedOperatorOverload(nullptr) {}

        AssignmentBinaryOperatorExpr(TextPosition startPosition, TextPosition endPosition,
                                     Expr* leftValue, Expr* rightValue, std::string nestedOperator)
                : Expr(Kind::AssignmentBinaryOperator, startPosition, endPosition),
                  leftValue(leftValue), rightValue(rightValue), _hasNestedOperator(true),
                  _nestedOperator(std::move(nestedOperator)), nestedOperatorOverload(nullptr) {}

        Expr* leftValue;
        Expr* rightValue;
        bool hasNestedOperator() const { return _hasNestedOperator; }
        std::string const& nestedOperator() const { return _nestedOperator; }
        /// If the nested operator is overloaded we store the declaration that will be called for the operator
        OperatorDecl* nestedOperatorOverload;

        Expr* deepCopy() const override {
            auto result = new AssignmentBinaryOperatorExpr(startPosition(), endPosition(),
                                                           leftValue->deepCopy(),
                                                           rightValue->deepCopy(),
                                                           _nestedOperator);
            // Set the `hasNestedOperator` since the constructor we're using always sets it to true
            result->_hasNestedOperator = _hasNestedOperator;
            if (resultType) {
                result->resultType = resultType->deepCopy();
            }
            result->isUnreachable = isUnreachable;
            result->nestedOperatorOverload = nestedOperatorOverload;
            return result;
        }

        ~AssignmentBinaryOperatorExpr() override {
            delete leftValue;
            delete rightValue;
        }

    private:
        /// `hasNestedOperator` tells us if we should apply the `nestedOperator` to the result of `left` and `right`
        bool _hasNestedOperator;
        /// The string representation of the nested operator (i.e. `+` for `+=`)
        std::string _nestedOperator;

    };
}

#endif //GULC_ASSIGNMENTBINARYOPERATOREXPR_HPP
