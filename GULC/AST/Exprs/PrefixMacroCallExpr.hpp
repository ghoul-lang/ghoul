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

#ifndef GULC_PREFIXMACROCALLEXPR_HPP
#define GULC_PREFIXMACROCALLEXPR_HPP

#include <AST/Expr.hpp>
#include <vector>

namespace gulc {
    /// This could either be a custom operator call (which are subsets of macros) or an actual macro call
    class PrefixMacroCallExpr : public Expr {
    public:
        static bool classof(const Expr *expr) { return expr->getExprKind() == Kind::PrefixMacroCall; }

        PrefixMacroCallExpr(TextPosition startPosition, TextPosition endPosition,
                            std::string macroName, std::vector<Expr*> templateArguments,
                            Expr* expr)
                : Expr(Kind::PrefixMacroCall, startPosition, endPosition),
                  _macroName(std::move(macroName)), templateArguments(std::move(templateArguments)),
                  expr(expr) {}

        std::string const& macroName() const { return _macroName; }
        std::vector<Expr*> templateArguments;
        Expr* expr;

        bool hasTemplateArguments() const { return !templateArguments.empty(); }

        Expr* deepCopy() const override {
            std::vector<Expr*> copiedTemplateArguments;

            for (Expr* templateArgument : templateArguments) {
                copiedTemplateArguments.push_back(templateArgument->deepCopy());
            }

            auto result = new PrefixMacroCallExpr(startPosition(), endPosition(),
                                                 _macroName, copiedTemplateArguments,
                                                  expr->deepCopy());
            if (resultType) {
                result->resultType = resultType->deepCopy();
            }
            result->isUnreachable = isUnreachable;
            return result;
        }

        ~PrefixMacroCallExpr() override {
            for (Expr* templateArgument : templateArguments) {
                delete templateArgument;
            }

            delete expr;
        }

    private:
        std::string _macroName;

    };
}

#endif //GULC_PREFIXMACROCALLEXPR_HPP
