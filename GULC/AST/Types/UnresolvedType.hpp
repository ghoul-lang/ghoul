#ifndef GULC_UNRESOLVEDTYPE_HPP
#define GULC_UNRESOLVEDTYPE_HPP

#include <AST/Type.hpp>
#include <string>
#include <vector>
#include <AST/Expr.hpp>

namespace gulc {
    class UnresolvedType : public Type {
    public:
        static bool classof(const Type *expr) { return expr->getTypeKind() == Kind::Unresolved; }

        UnresolvedType(TextPosition startPosition, TextPosition endPosition, std::vector<std::string> namespacePath,
                       std::string name, std::vector<Expr*> templateArguments)
                : Type(Kind::Unresolved, startPosition, endPosition),
                  _namespacePath(std::move(namespacePath)), _name(std::move(name)),
                  _templateArguments(std::move(templateArguments)) {}

        const std::vector<std::string>& namespacePath() const { return _namespacePath; }
        std::string name() const { return _name; }
        std::vector<Expr*>& templateArguments() { return _templateArguments; }
        const std::vector<Expr*>& templateArguments() const { return _templateArguments; }
        bool hasTemplateArguments() const { return !_templateArguments.empty(); }

        ~UnresolvedType() override {
            for (Expr* templateArgument : _templateArguments) {
                delete templateArgument;
            }
        }

    private:
        std::vector<std::string> _namespacePath;
        std::string _name;
        std::vector<Expr*> _templateArguments;

    };
}

#endif //GULC_UNRESOLVEDTYPE_HPP
