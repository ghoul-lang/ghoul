#ifndef GULC_TEMPLATETYPENAMETYPE_HPP
#define GULC_TEMPLATETYPENAMETYPE_HPP

#include <AST/Type.hpp>
#include <vector>

namespace gulc {
    class TemplateTypenameType : public Type {
    public:
        static bool classof(const Type *expr) { return expr->getTypeKind() == TypeKind::TemplateTypename; }

        TemplateTypenameType(TextPosition startPosition, TextPosition endPosition)
                : Type(TypeKind::TemplateTypename, startPosition, endPosition) {}

    };
}

#endif //GULC_TEMPLATETYPENAMETYPE_HPP
