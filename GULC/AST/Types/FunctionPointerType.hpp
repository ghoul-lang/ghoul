#ifndef GULC_FUNCTIONPOINTERTYPE_HPP
#define GULC_FUNCTIONPOINTERTYPE_HPP

#include <AST/Type.hpp>
#include <vector>

namespace gulc {
    class FunctionPointerType : public Type {
    public:
        static bool classof(const Type *expr) { return expr->getTypeKind() == Kind::FunctionPointer; }

        FunctionPointerType(TextPosition startPosition, TextPosition endPosition,
                            Type* resultType, std::vector<Type*> paramTypes)
                : Type(Kind::FunctionPointer, startPosition, endPosition),
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

        ~FunctionPointerType() override {
            delete resultType;

            for (Type* paramType : paramTypes) {
                delete paramType;
            }
        }

    };
}

#endif //GULC_FUNCTIONPOINTERTYPE_HPP
