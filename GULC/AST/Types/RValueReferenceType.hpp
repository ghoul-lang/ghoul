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

#ifndef GULC_RVALUEREFERENCETYPE_HPP
#define GULC_RVALUEREFERENCETYPE_HPP

#include <AST/Type.hpp>

namespace gulc {
    class RValueReferenceType : public Type {
    public:
        static bool classof(const Type *expr) { return expr->getTypeKind() == Kind::RValueReference; }

        RValueReferenceType(TextPosition startPosition, TextPosition endPosition, Type* referenceToType)
                : Type(Kind::RValueReference, startPosition, endPosition), referenceToType(referenceToType) {}

        Type *referenceToType;

        std::string getString() const override { return referenceToType->getString() + "&"; }

        ~RValueReferenceType() override {
            delete referenceToType;
        }

    };
}

#endif //GULC_RVALUEREFERENCETYPE_HPP
