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

#ifndef GULC_STRUCTDECL_HPP
#define GULC_STRUCTDECL_HPP

#include <AST/Decl.hpp>
#include <vector>
#include "GlobalVariableDecl.hpp"
#include "ConstructorDecl.hpp"
#include "DestructorDecl.hpp"

namespace gulc {
    class StructDecl : public Decl {
    public:
        static bool classof(const Decl *decl) { return decl->getDeclKind() == Kind::Struct; }

        StructDecl(std::string name, std::string sourceFile, TextPosition startPosition, TextPosition endPosition,
                   std::vector<ConstructorDecl*> constructors, std::vector<Decl*> members, DestructorDecl* destructor)
                : Decl(Kind::Struct, std::move(name), std::move(sourceFile), startPosition, endPosition),
                  constructors(std::move(constructors)), members(std::move(members)), destructor(destructor),
                  _hasEmptyConstructor(false) {
            // Search to see if any of the constructors are empty
            for (ConstructorDecl* constructor : this->constructors) {
                if (constructor->parameters.empty()) {
                    _hasEmptyConstructor = true;
                    break;
                }
            }
        }

        // These are just sorted references to already existing members within the `members` list, we do NOT free these as that would cause a double free issue.
        std::vector<GlobalVariableDecl*> dataMembers;

        std::vector<ConstructorDecl*> constructors;
        std::vector<Decl*> members;
        // There can only be one destructor
        DestructorDecl* destructor;

        bool hasEmptyConstructor() const { return _hasEmptyConstructor; }

        Decl* deepCopy() const override {
            std::vector<ConstructorDecl*> copiedConstructors{};
            std::vector<Decl*> copiedMembers{};
            DestructorDecl* copiedDestructor = nullptr;

            for (ConstructorDecl* constructor : constructors) {
                copiedConstructors.push_back(llvm::dyn_cast<ConstructorDecl>(constructor->deepCopy()));
            }

            for (Decl* decl : members) {
                copiedMembers.push_back(decl->deepCopy());
            }

            if (destructor) {
                copiedDestructor = llvm::dyn_cast<DestructorDecl>(destructor->deepCopy());
            }

            auto result = new StructDecl(name(), sourceFile(),
                                         startPosition(), endPosition(),
                                         std::move(copiedConstructors), std::move(copiedMembers), copiedDestructor);
            result->parentNamespace = parentNamespace;
            result->parentStruct = parentStruct;

            return result;
        }

        ~StructDecl() override {
            delete destructor;

            for (ConstructorDecl* constructor : constructors) {
                delete constructor;
            }

            for (Decl* decl : members) {
                delete decl;
            }
        }

    private:
        // This is needed to be able to declare local variable structs without constructor arguments (i.e. `TestStruct v;` instead of `TestStruct v(arg1, arg2, etc);`
        bool _hasEmptyConstructor;

    };
}

#endif //GULC_STRUCTDECL_HPP
