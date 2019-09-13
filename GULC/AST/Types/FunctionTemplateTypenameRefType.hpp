#ifndef GULC_FUNCTIONTEMPLATETYPENAMEREFTYPE_HPP
#define GULC_FUNCTIONTEMPLATETYPENAMEREFTYPE_HPP

#include <AST/Type.hpp>
#include <string>

namespace gulc {
    class FunctionTemplateTypenameRefType : public Type {
    public:
        static bool classof(const Type *expr) { return expr->getTypeKind() == Kind::FunctionTemplateTypenameRef; }

        FunctionTemplateTypenameRefType(TextPosition startPosition, TextPosition endPosition, std::string name)
                : Type(Kind::FunctionTemplateTypenameRef, startPosition, endPosition),
                  _name(std::move(name)) {}

        std::string name() const { return _name; }
        std::string getString() const override { return _name; }

    private:
        std::string _name;

    };
}

#endif //GULC_FUNCTIONTEMPLATETYPENAMEREFTYPE_HPP
