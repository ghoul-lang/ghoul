// Copyright (C) 2019 Michael Brandon Huddle
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as published
// by the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public License
// along with this program.  If not, see <https://www.gnu.org/licenses/>.

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

        UnresolvedType(TextPosition startPosition, TextPosition endPosition, TypeQualifier qualifier,
                       std::vector<std::string> namespacePath,
                       std::string name, std::vector<Expr*> templateArguments)
                : Type(Kind::Unresolved, startPosition, endPosition, qualifier),
                  _namespacePath(std::move(namespacePath)), _name(std::move(name)),
                  _templateArguments(std::move(templateArguments)) {}

        const std::vector<std::string>& namespacePath() const { return _namespacePath; }
        std::string name() const { return _name; }
        std::vector<Expr*>& templateArguments() { return _templateArguments; }
        const std::vector<Expr*>& templateArguments() const { return _templateArguments; }
        bool hasTemplateArguments() const { return !_templateArguments.empty(); }

        std::string getString() const override {
            std::string result;

            for (const std::string& namespacePathItem : namespacePath()) {
                result += namespacePathItem + ".";
            }

            result += _name;

            if (hasTemplateArguments()) {
                result += "<...>";
            }

            return result;
        }

        Type* deepCopy() const override {
            std::vector<std::string> copiedNamespacePath;
            std::vector<Expr*> copiedTemplateArguments;

            for (std::string namespacePathPart : _namespacePath) {
                copiedNamespacePath.push_back(std::move(namespacePathPart));
            }

            for (Expr* templateArgument : _templateArguments) {
                copiedTemplateArguments.push_back(templateArgument->deepCopy());
            }

            return new UnresolvedType(startPosition(), endPosition(), qualifier(),
                                      std::move(copiedNamespacePath), _name, std::move(copiedTemplateArguments));
        }

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
