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

#ifndef GULC_RESOLVEDTYPEREFEXPR_HPP
#define GULC_RESOLVEDTYPEREFEXPR_HPP

#include <AST/Expr.hpp>
#include <AST/Type.hpp>
#include <vector>

namespace gulc {
    class ResolvedTypeRefExpr : public Expr {
    public:
        static bool classof(const Expr *expr) { return expr->getExprKind() == Kind::ResolvedTypeRef; }

        ResolvedTypeRefExpr(TextPosition startPosition, TextPosition endPosition,
                            Type* resolvedType)
                : Expr(Kind::ResolvedTypeRef, startPosition, endPosition),
                  resolvedType(resolvedType) {}

        Type* resolvedType;

        Expr* deepCopy() const override {
            auto result = new ResolvedTypeRefExpr(startPosition(), endPosition(),
                                                  resultType->deepCopy());
            if (resultType) {
                result->resultType = resultType->deepCopy();
            }
            result->isUnreachable = isUnreachable;
            return result;
        }

        ~ResolvedTypeRefExpr() override {
            delete resolvedType;
        }

    };
}

#endif //GULC_RESOLVEDTYPEREFEXPR_HPP
