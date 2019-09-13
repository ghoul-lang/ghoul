#ifndef GULC_POINTERTYPE_HPP
#define GULC_POINTERTYPE_HPP

#include <AST/Type.hpp>

namespace gulc {
    class PointerType : public Type {
    public:
        static bool classof(const Type *expr) { return expr->getTypeKind() == Kind::Pointer; }

        PointerType(TextPosition startPosition, TextPosition endPosition, Type* pointToType)
                : Type(Kind::Pointer, startPosition, endPosition),
                  pointToType(pointToType) {}

        Type* pointToType;
        std::string getString() const override { return pointToType->getString() + "*"; }

        ~PointerType() override {
            delete pointToType;
        }

    };
}

#endif //GULC_POINTERTYPE_HPP
