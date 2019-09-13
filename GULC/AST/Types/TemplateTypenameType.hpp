#ifndef GULC_TEMPLATETYPENAMETYPE_HPP
#define GULC_TEMPLATETYPENAMETYPE_HPP

#include <AST/Type.hpp>
#include <vector>

namespace gulc {
    class TemplateTypenameType : public Type {
    public:
        static bool classof(const Type *expr) { return expr->getTypeKind() == Kind::TemplateTypename; }

        TemplateTypenameType(TextPosition startPosition, TextPosition endPosition)
                : Type(Kind::TemplateTypename, startPosition, endPosition) {}

        std::string getString() const override { return "typename"; }
    };
}

#endif //GULC_TEMPLATETYPENAMETYPE_HPP
