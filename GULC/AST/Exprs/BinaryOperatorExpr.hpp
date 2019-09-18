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
