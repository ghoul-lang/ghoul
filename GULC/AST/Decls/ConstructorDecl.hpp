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
#include <AST/Exprs/BaseConstructorCallExpr.hpp>
#include "ParameterDecl.hpp"

namespace gulc {
    class ConstructorDecl : public Decl {
    public:
        static bool classof(const Decl *decl) { return decl->getDeclKind() == Kind::Constructor; }

        ConstructorDecl(std::vector<Attr*> attributes, std::string name, std::string sourceFile,
                        TextPosition startPosition, TextPosition endPosition,
                        Visibility visibility, std::vector<ParameterDecl*> parameters,
                        BaseConstructorCallExpr* baseConstructorCall, CompoundStmt* body)
                : Decl(Kind::Constructor, std::move(attributes), std::move(name), std::move(sourceFile),
                       startPosition, endPosition, visibility),
                  parameters(std::move(parameters)), baseConstructor(nullptr),
                  baseConstructorCall(baseConstructorCall), _body(body), isUserSpecified(true) {}

        std::vector<ParameterDecl*> parameters;
        bool hasParameters() const { return !parameters.empty(); }
        CompoundStmt* body() const { return _body; }

        std::string mangledNameVTable() const { return _mangledNameVTable; }
        void setMangledNameVTable(std::string const& mangledName) { _mangledNameVTable = mangledName; }

        // This is used for calling a base struct's constructor
        // We don't own this so we don't free it
        ConstructorDecl* baseConstructor;
        // This is the actual call to the base constructor. We DO own this and MUST free it
        BaseConstructorCallExpr* baseConstructorCall;

        // This tells us if this is a constructor made by the programmer in the source code or if it is a compiler
        // generated constructor
        bool isUserSpecified;

        Decl* deepCopy() const override {
            std::vector<Attr*> copiedAttributes;
            std::vector<ParameterDecl*> copiedParameters;
            BaseConstructorCallExpr* copiedBaseConstructorCall = nullptr;

            for (Attr* attribute : _attributes) {
                copiedAttributes.push_back(attribute->deepCopy());
            }

            for (ParameterDecl* parameter : parameters) {
                copiedParameters.push_back(static_cast<ParameterDecl*>(parameter->deepCopy()));
            }

            if (baseConstructorCall != nullptr) {
                copiedBaseConstructorCall = llvm::dyn_cast<BaseConstructorCallExpr>(baseConstructorCall->deepCopy());
            }

            auto result = new ConstructorDecl(copiedAttributes, name(), sourceFile(),
                                              startPosition(), endPosition(),
                                              visibility(),
                                              std::move(copiedParameters),
                                              copiedBaseConstructorCall,
                                              static_cast<CompoundStmt*>(_body->deepCopy()));
            result->parentNamespace = parentNamespace;
            result->parentStruct = parentStruct;
            result->baseConstructor = baseConstructor;
            result->_mangledNameVTable = _mangledNameVTable;
            result->isUserSpecified = isUserSpecified;

            return result;
        }

        ~ConstructorDecl() override {
            for (ParameterDecl* parameter : parameters) {
                delete parameter;
            }

            delete baseConstructorCall;

            delete _body;
        }

    private:
        CompoundStmt* _body;
        // We force `CodeGen` to generate the vtable variant of every constructor instead of duplicating constructors.
        // So we have a separate mangled name here for the vtable setting constructor
        std::string _mangledNameVTable;

    };
}

#endif //GULC_CONSTRUCTORDECL_HPP
