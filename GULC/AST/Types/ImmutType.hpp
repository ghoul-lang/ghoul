#ifndef GULC_IMMUTTYPE_HPP
#define GULC_IMMUTTYPE_HPP

#include <AST/Type.hpp>

namespace gulc {
    class ImmutType : public Type {
    public:
        static bool classof(const Type *expr) { return expr->getTypeKind() == Kind::Immut; }

        ImmutType(TextPosition startPosition, TextPosition endPosition, Type* pointToType)
                : Type(Kind::Immut, startPosition, endPosition),
                  pointToType(pointToType) {}

        Type* pointToType;
        std::string getString() const override { return pointToType->getString() + " immut"; }

        ~ImmutType() override {
            delete pointToType;
        }

    };
}

#endif //GULC_IMMUTTYPE_HPP
