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

#ifndef GULC_NAMESPACEDECL_HPP
#define GULC_NAMESPACEDECL_HPP

#include <AST/Decl.hpp>
#include <vector>

namespace gulc {
    class NamespaceDecl : public Decl {
    public:
        static bool classof(const Decl *decl) { return decl->getDeclKind() == Kind::Namespace; }

        NamespaceDecl(std::string name, std::string sourceFile, TextPosition startPosition, TextPosition endPosition)
                : Decl(Kind::Namespace, std::move(name), std::move(sourceFile), startPosition, endPosition,
                       Visibility::Unspecified),
                  _nestedDecls(), _isPrototype(false) {}

        void makePrototype() { _isPrototype = true; }

        std::vector<Decl*>& nestedDecls() { return _nestedDecls; }
        const std::vector<Decl*>& nestedDecls() const { return _nestedDecls; }

        void addNestedDecl(Decl* decl) { _nestedDecls.push_back(decl); }

        Decl* deepCopy() const override {
            NamespaceDecl* result = new NamespaceDecl(name(), sourceFile(),
                                                      startPosition(), endPosition());

            if (_isPrototype) {
                result->makePrototype();
            }

            std::vector<Decl*>& newNestedDecls = result->nestedDecls();

            for (Decl* nestedDecl : _nestedDecls) {
                newNestedDecls.push_back(nestedDecl->deepCopy());
            }

            result->parentNamespace = parentNamespace;
            result->parentStruct = parentStruct;
            return result;
        }

        ~NamespaceDecl() override {
            for (Decl* decl : _nestedDecls) {
                if (_isPrototype && !llvm::isa<NamespaceDecl>(decl)) {
                    // We're not allowed to delete anything but namespaces if the namespace is a prototype
                    continue;
                }

                delete decl;
            }
        }

    private:
        std::vector<Decl*> _nestedDecls;
        // If this is true it means we only own a nested `Decl` if it is a namespace, all other cannot be deleted by us.
        bool _isPrototype;

    };
}

#endif //GULC_NAMESPACEDECL_HPP
