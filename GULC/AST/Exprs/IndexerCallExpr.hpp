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

#ifndef GULC_INDEXERCALLEXPR_HPP
#define GULC_INDEXERCALLEXPR_HPP

#include <AST/Expr.hpp>
#include <vector>

namespace gulc {
    class IndexerCallExpr : public Expr {
    public:
        static bool classof(const Expr *expr) { return expr->getExprKind() == Kind::IndexerCall; }

        IndexerCallExpr(TextPosition startPosition, TextPosition endPosition,
                Expr* indexerReference, std::vector<Expr*> arguments)
                : Expr(Kind::IndexerCall, startPosition, endPosition),
                  indexerReference(indexerReference), _arguments(std::move(arguments)) {}

        Expr* indexerReference;
        std::vector<Expr*>& arguments() { return _arguments; }
        const std::vector<Expr*>& arguments() const { return _arguments; }
        bool hasArguments() const { return !_arguments.empty(); }

        ~IndexerCallExpr() override {
            delete indexerReference;

            for (Expr* argument : _arguments) {
                delete argument;
            }
        }

    private:
        std::vector<Expr*> _arguments;

    };
}

#endif //GULC_INDEXERCALLEXPR_HPP
