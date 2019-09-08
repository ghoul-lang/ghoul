#ifndef GULC_BUILTINTYPE_HPP
#define GULC_BUILTINTYPE_HPP

#include <AST/Type.hpp>
#include <string>

namespace gulc {
    class BuiltInType : public Type {
    public:
        static bool classof(const Type *expr) { return expr->getTypeKind() == Kind::BuiltIn; }

        BuiltInType(TextPosition startPosition, TextPosition endPosition, std::string name)
                : Type(Kind::BuiltIn, startPosition, endPosition),
                  _name(std::move(name)) {}

        std::string name() const { return _name; }

    private:
        std::string _name;

    };
}

#endif //GULC_BUILTINTYPE_HPP
