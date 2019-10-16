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

#ifndef GULC_TEMPNAMESPACEREFEXPR_HPP
#define GULC_TEMPNAMESPACEREFEXPR_HPP

#include <AST/Expr.hpp>
#include <AST/Decls/NamespaceDecl.hpp>

namespace gulc {
    class TempNamespaceRefExpr : public Expr {
    public:
        static bool classof(const Expr *expr) { return expr->getExprKind() == Kind::TempNamespaceRef; }

        TempNamespaceRefExpr(TextPosition startPosition, TextPosition endPosition, NamespaceDecl* namespacePrototype)
                : Expr(Kind::TempNamespaceRef, startPosition, endPosition),
                  _namespacePrototype(namespacePrototype) {}

        NamespaceDecl* namespaceDecl() { return _namespacePrototype; }

        Expr* deepCopy() const override {
            return new TempNamespaceRefExpr(startPosition(), endPosition(),
                                            static_cast<NamespaceDecl*>(_namespacePrototype->deepCopy()));
        }

        // We don't own `_namespacePrototype`. We don't free it.

    private:
        NamespaceDecl* _namespacePrototype;

    };
}

#endif //GULC_TEMPNAMESPACEREFEXPR_HPP
