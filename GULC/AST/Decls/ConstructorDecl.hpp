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

#ifndef GULC_CONSTRUCTORDECL_HPP
#define GULC_CONSTRUCTORDECL_HPP

#include <AST/Decl.hpp>
#include <AST/Stmts/CompoundStmt.hpp>
#include "ParameterDecl.hpp"

namespace gulc {
    class ConstructorDecl : public Decl {
    public:
        static bool classof(const Decl *decl) { return decl->getDeclKind() == Kind::Constructor; }

        ConstructorDecl(std::string name, std::string sourceFile, TextPosition startPosition, TextPosition endPosition,
                        std::vector<ParameterDecl*> parameters, CompoundStmt* body)
                : Decl(Kind::Constructor, std::move(name), std::move(sourceFile), startPosition, endPosition),
                  parameters(std::move(parameters)), _body(body), _assignsVTable(false) {}

        std::vector<ParameterDecl*> parameters;
        bool hasParameters() const { return !parameters.empty(); }
        CompoundStmt* body() const { return _body; }
        bool assignsVTable() const { return _assignsVTable; }

        Decl* deepCopy() const override {
            std::vector<ParameterDecl*> copiedParameters;

            for (ParameterDecl* parameter : parameters) {
                copiedParameters.push_back(static_cast<ParameterDecl*>(parameter->deepCopy()));
            }

            auto result = new ConstructorDecl(name(), sourceFile(),
                                              startPosition(), endPosition(),
                                              std::move(copiedParameters),
                                              static_cast<CompoundStmt*>(_body->deepCopy()));
            result->parentNamespace = parentNamespace;
            result->parentStruct = parentStruct;
            return result;
        }

        ~ConstructorDecl() override {
            for (ParameterDecl* parameter : parameters) {
                delete parameter;
            }

            delete _body;
        }

    private:
        CompoundStmt* _body;
        // This is used to differentiate between constructors that can be called as a base constructor and ones that cannot
        bool _assignsVTable;

    };
}

#endif //GULC_CONSTRUCTORDECL_HPP
