#ifndef GULC_CONSTTYPE_HPP
#define GULC_CONSTTYPE_HPP

#include <AST/Type.hpp>

namespace gulc {
    class ConstType : public Type {
    public:
        static bool classof(const Type *expr) { return expr->getTypeKind() == Kind::Const; }

        ConstType(TextPosition startPosition, TextPosition endPosition, Type* pointToType)
                : Type(Kind::Const, startPosition, endPosition),
                  pointToType(pointToType) {}

        Type* pointToType;
        std::string getString() const override { return pointToType->getString() + " const"; }

        ~ConstType() override {
            delete pointToType;
        }

    };
}

#endif //GULC_CONSTTYPE_HPP
