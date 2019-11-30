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

#ifndef GULC_STRUCTTYPE_HPP
#define GULC_STRUCTTYPE_HPP

#include <AST/Type.hpp>
#include <AST/Decls/StructDecl.hpp>

namespace gulc {
    class StructType : public Type {
    public:
        static bool classof(const Type *expr) { return expr->getTypeKind() == Kind::Struct; }

        StructType(TextPosition startPosition, TextPosition endPosition, TypeQualifier qualifier,
                   std::string name, StructDecl* decl)
                : Type(Kind::Struct, startPosition, endPosition, qualifier),
                  _name(std::move(name)), _decl(decl), _doVTableCalls(true) {}

        StructDecl* decl() const { return _decl; }

        std::string getString() const override { return _name; }

        bool doVTableCalls() const { return _doVTableCalls; }
        void setDoVTableCalls(bool val) { _doVTableCalls = val; }

        Type* deepCopy() const override {
            auto result = new StructType(startPosition(), endPosition(), qualifier(),
                                         _name, _decl);
            result->setDoVTableCalls(_doVTableCalls);
            return result;
        }

    private:
        std::string _name;
        // We don't own this so we don't free it.
        StructDecl* _decl;
        // In some scenarios (mainly just with `base`) we don't perform vtable calls, we call the straight function
        bool _doVTableCalls;

    };
}

#endif //GULC_STRUCTTYPE_HPP
