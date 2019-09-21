#ifndef GULC_MUTTYPE_HPP
#define GULC_MUTTYPE_HPP

#include <AST/Type.hpp>

namespace gulc {
    class MutType : public Type {
    public:
        static bool classof(const Type *expr) { return expr->getTypeKind() == Kind::Mut; }

        MutType(TextPosition startPosition, TextPosition endPosition, Type* pointToType)
                : Type(Kind::Mut, startPosition, endPosition),
                  pointToType(pointToType) {}

        Type* pointToType;
        std::string getString() const override { return pointToType->getString() + " mut"; }

        ~MutType() override {
            delete pointToType;
        }

    };
}

#endif //GULC_MUTTYPE_HPP
