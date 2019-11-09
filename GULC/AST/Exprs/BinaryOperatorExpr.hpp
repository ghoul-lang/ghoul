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

#ifndef GULC_BINARYOPERATOREXPR_HPP
#define GULC_BINARYOPERATOREXPR_HPP

#include <AST/Expr.hpp>
#include <string>

namespace gulc {
    class BinaryOperatorExpr : public Expr {
    public:
        static bool classof(const Expr *expr) { return expr->getExprKind() == Kind::BinaryOperator; }

        BinaryOperatorExpr(TextPosition startPosition, TextPosition endPosition,
                           std::string operatorName, Expr* leftValue, Expr* rightValue)
                : Expr(Kind::BinaryOperator, startPosition, endPosition),
			      leftValue(leftValue), rightValue(rightValue), _operatorName(std::move(operatorName)),
                  _isBuiltInAssignmentOperator(false) {
            if (_operatorName == "=" || _operatorName == ">>=" || _operatorName == "<<=" || _operatorName == "+=" ||
                _operatorName == "-=" || _operatorName == "*=" || _operatorName == "/=" || _operatorName == "%=" ||
                _operatorName == "&=" || _operatorName == "|=" || _operatorName == "^=") {
                _isBuiltInAssignmentOperator = true;
            }
        }

        std::string operatorName() const { return _operatorName; }
        Expr* leftValue;
        Expr* rightValue;
        bool isBuiltInAssignmentOperator() const { return _isBuiltInAssignmentOperator; }

        void setOperatorName(const std::string& operatorName) {
            _operatorName = operatorName;

            _isBuiltInAssignmentOperator =
                    _operatorName == "=" || _operatorName == ">>=" || _operatorName == "<<=" || _operatorName == "+=" ||
                    _operatorName == "-=" || _operatorName == "*=" || _operatorName == "/=" || _operatorName == "%=" ||
                    _operatorName == "&=" || _operatorName == "|=" || _operatorName == "^=";
        }

        Expr* deepCopy() const override {
            auto result = new BinaryOperatorExpr(startPosition(), endPosition(),
                                                 _operatorName,
                                                 leftValue->deepCopy(), rightValue->deepCopy());
            if (resultType) {
                result->resultType = resultType->deepCopy();
            }
            result->isUnreachable = isUnreachable;
            return result;
        }

        ~BinaryOperatorExpr() override {
            delete leftValue;
            delete rightValue;
        }

    private:
        std::string _operatorName;
        bool _isBuiltInAssignmentOperator;

    };
}

#endif //GULC_BINARYOPERATOREXPR_HPP
