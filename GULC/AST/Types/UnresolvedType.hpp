#ifndef GULC_UNRESOLVEDTYPE_HPP
#define GULC_UNRESOLVEDTYPE_HPP

#include <AST/Type.hpp>
#include <string>
#include <vector>

namespace gulc {
    class UnresolvedType : public Type {
    public:
        static bool classof(const Type *expr) { return expr->getTypeKind() == TypeKind::Unresolved; }

        UnresolvedType(TextPosition startPosition, TextPosition endPosition, std::vector<std::string> namespacePath,
                       std::string name)
                : Type(TypeKind::Unresolved, startPosition, endPosition),
                  _namespacePath(std::move(namespacePath)), _name(std::move(name)) {}

        const std::vector<std::string>& namespacePath() const { return _namespacePath; }
        std::string name() const { return _name; }

    private:
        std::vector<std::string> _namespacePath;
        std::string _name;

    };
}

#endif //GULC_UNRESOLVEDTYPE_HPP
