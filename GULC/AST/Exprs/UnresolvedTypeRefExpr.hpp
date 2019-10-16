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

#ifndef GULC_UNRESOLVEDTYPEREFEXPR_HPP
#define GULC_UNRESOLVEDTYPEREFEXPR_HPP

#include <AST/Expr.hpp>
#include <AST/Type.hpp>
#include <vector>

namespace gulc {
    class UnresolvedTypeRefExpr : public Expr {
    public:
        static bool classof(const Expr *expr) { return expr->getExprKind() == Kind::UnresolvedTypeRef; }

        // TODO: Do we need to have template arguments?
        UnresolvedTypeRefExpr(TextPosition startPosition, TextPosition endPosition,
                              Type* unresolvedType)
                : Expr(Kind::UnresolvedTypeRef, startPosition, endPosition),
                  unresolvedType(unresolvedType) {}

        Type* unresolvedType;

        Expr* deepCopy() const override {
            return new UnresolvedTypeRefExpr(startPosition(), endPosition(),
                                             unresolvedType->deepCopy());
        }

        ~UnresolvedTypeRefExpr() override {
            delete unresolvedType;
        }

    };
}

#endif //GULC_UNRESOLVEDTYPEREFEXPR_HPP
