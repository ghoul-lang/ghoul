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

#ifndef GULC_DESTRUCTORDECL_HPP
#define GULC_DESTRUCTORDECL_HPP

#include <AST/Decl.hpp>
#include <AST/Stmts/CompoundStmt.hpp>
#include "FunctionDecl.hpp"

namespace gulc {
    class DestructorDecl : public Decl {
    public:
        static bool classof(const Decl *decl) { return decl->getDeclKind() == Kind::Destructor; }

        DestructorDecl(std::vector<Attr*> attributes, std::string name, std::string sourceFile,
                       TextPosition startPosition, TextPosition endPosition, FunctionModifiers modifier,
                       CompoundStmt *body)
                : Decl(Kind::Destructor, std::move(attributes), std::move(name), std::move(sourceFile),
                       startPosition, endPosition, Visibility::Unspecified),
                  baseDestructor(nullptr), _body(body), _modifier(modifier) {}

        CompoundStmt *body() const { return _body; }

        FunctionModifiers modifier() const { return _modifier; }

        // Returns true if the function is `abstract`, `virtual`, or `override`
        bool isVirtual() const {
            return _modifier == FunctionModifiers::Abstract || _modifier == FunctionModifiers::Virtual ||
                   _modifier == FunctionModifiers::Override;
        }

        // This is used for calling a base struct's destructor
        // We don't own this so we don't free it
        DestructorDecl* baseDestructor;

        Decl *deepCopy() const override {
            std::vector<Attr*> copiedAttributes;

            for (Attr* attribute : _attributes) {
                copiedAttributes.push_back(attribute->deepCopy());
            }

            auto result = new DestructorDecl(copiedAttributes, name(), sourceFile(),
                                             startPosition(), endPosition(),
                                             _modifier,
                                             static_cast<CompoundStmt*>(_body->deepCopy()));
            result->parentNamespace = parentNamespace;
            result->parentStruct = parentStruct;
            result->baseDestructor = baseDestructor;
            return result;
        }

        ~DestructorDecl() override {
            delete _body;
        }

    private:
        CompoundStmt *_body;
        FunctionModifiers _modifier;

    };
}

#endif //GULC_DESTRUCTORDECL_HPP
