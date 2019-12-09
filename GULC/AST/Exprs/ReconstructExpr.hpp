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

#ifndef GULC_RECONSTRUCTEXPR_HPP
#define GULC_RECONSTRUCTEXPR_HPP

#include <AST/Expr.hpp>
#include <AST/Decls/ConstructorDecl.hpp>

namespace gulc {
    /**
     * Used mainly for calling move and copy constructors, the `ReconstructExpr` is used to call a constructor on an
     * existing struct reference. It can be configured to destruct the old reference before reconstructing it or it
     * can be called without destructing
     */
    class ReconstructExpr : public Expr {
    public:
        static bool classof(const Expr *expr) { return expr->getExprKind() == Kind::Reconstruct; }

        ReconstructExpr(TextPosition startPosition, TextPosition endPosition,
                        ConstructorDecl* constructor, Expr* thisRef, std::vector<Expr*> arguments,
                        bool destructThisRef)
                : Expr(Kind::Reconstruct, startPosition, endPosition),
                  constructor(constructor), thisRef(thisRef), arguments(std::move(arguments)),
                  destructThisRef(destructThisRef) {}

        // The constructor we reconstruct with
        // We don't own this so we don't free it
        ConstructorDecl* constructor;
        // The `this` reference
        Expr* thisRef;
        // The arguments we pass to the constructor, except for `this`
        std::vector<Expr*> arguments;
        // Whether we should destruct the `thisRef` expression or not
        // NOTE: The only reason this exists is due to the fact that GUL doesn't allow constructing struct member
        //       variables in the base class area. You can't do `Constructor(StructType s) : _s(s)` so you have
        //       to set it as `{ _s = s }` which if we destruct `_s` we will pass garbage to the destructor, which
        //       we don't want
        bool destructThisRef;

        Expr* deepCopy() const override {
            std::vector<Expr*> copiedArguments;

            for (Expr* argument : arguments) {
                copiedArguments.push_back(argument->deepCopy());
            }

            auto result = new ReconstructExpr(startPosition(), endPosition(),
                                              constructor, thisRef->deepCopy(),
                                              copiedArguments, destructThisRef);
            if (resultType) {
                result->resultType = resultType->deepCopy();
            }
            result->isUnreachable = isUnreachable;
            return result;
        }

        ~ReconstructExpr() override {
            delete thisRef;

            for (Expr* argument : arguments) {
                delete argument;
            }
        }
    };
}

#endif //GULC_RECONSTRUCTEXPR_HPP
