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

#ifndef GULC_EXPR_HPP
#define GULC_EXPR_HPP

#include "Stmt.hpp"
#include "Type.hpp"

namespace gulc {
    class Expr : public Stmt {
    public:
        static bool classof(const Stmt *stmt) { return stmt->getStmtKind() == Stmt::Kind::Expr; }

        enum class Kind {
            BinaryOperator,
            PostfixOperator,
            PrefixOperator,

            Ternary,

            FunctionCall,
            IndexerCall,
            MemberAccessCall,

            Identifier,

            Paren,

            IntegerLiteral,
            FloatLiteral,
            StringLiteral,
            CharacterLiteral,

            PotentialExplicitCast,
            ExplicitCast,
            ImplicitCast,
            LValueToRValue,

            LocalVariableDeclOrPrefixOperatorCallExpr,

            ResolvedTypeRef,
            UnresolvedTypeRef,

            LocalVariableDecl
        };

        Kind getExprKind() const { return _kind; }

        // This is the type of this expression. I.e `(float)i + 1` will make the resultType of the 'BinaryOperatorExpr' 'float'
        Type* resultType;

        ~Expr() override {
            delete resultType;
        }

    protected:
        Expr(Kind kind, TextPosition startPosition, TextPosition endPosition)
                : Stmt(Stmt::Kind::Expr, startPosition, endPosition), resultType(nullptr), _kind(kind) {}

    private:
        const Kind _kind;

    };
}

#endif //GULC_EXPR_HPP
