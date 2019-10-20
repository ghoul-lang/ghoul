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

#ifndef GULC_CODEVERIFIER_HPP
#define GULC_CODEVERIFIER_HPP

#include <AST/FileAST.hpp>
#include <AST/Decls/FunctionDecl.hpp>
#include <AST/Exprs/LocalVariableDeclExpr.hpp>
#include <AST/Stmts/BreakStmt.hpp>
#include <AST/Stmts/CaseStmt.hpp>
#include <AST/Stmts/ContinueStmt.hpp>
#include <AST/Stmts/DoStmt.hpp>
#include <AST/Stmts/ForStmt.hpp>
#include <AST/Stmts/GotoStmt.hpp>
#include <AST/Stmts/IfStmt.hpp>
#include <AST/Stmts/LabeledStmt.hpp>
#include <AST/Stmts/ReturnStmt.hpp>
#include <AST/Stmts/SwitchStmt.hpp>
#include <AST/Stmts/TryStmt.hpp>
#include <AST/Stmts/WhileStmt.hpp>
#include <AST/Exprs/CharacterLiteralExpr.hpp>
#include <AST/Exprs/ExplicitCastExpr.hpp>
#include <AST/Exprs/FloatLiteralExpr.hpp>
#include <AST/Exprs/FunctionCallExpr.hpp>
#include <AST/Exprs/ImplicitCastExpr.hpp>
#include <AST/Exprs/IndexerCallExpr.hpp>
#include <AST/Exprs/IntegerLiteralExpr.hpp>
#include <AST/Exprs/MemberAccessCallExpr.hpp>
#include <AST/Exprs/ParenExpr.hpp>
#include <AST/Exprs/PostfixOperatorExpr.hpp>
#include <AST/Exprs/PotentialExplicitCastExpr.hpp>
#include <AST/Exprs/PrefixOperatorExpr.hpp>
#include <AST/Exprs/ResolvedTypeRefExpr.hpp>
#include <AST/Exprs/StringLiteralExpr.hpp>
#include <AST/Exprs/TernaryExpr.hpp>
#include <AST/Exprs/LValueToRValueExpr.hpp>
#include <AST/Decls/GlobalVariableDecl.hpp>
#include <AST/Decls/NamespaceDecl.hpp>
#include <AST/Decls/TemplateFunctionDecl.hpp>

namespace gulc {
    class CodeVerifier {
    public:
        CodeVerifier()
                : currentFileAst(nullptr), currentNamespace(nullptr), currentFunctionReturnType(nullptr),
                  currentFunctionTemplateParameters(nullptr), currentFunctionParameters(nullptr),
                  currentFunctionLocalVariablesCount(0), currentFunctionLocalVariables() {}

        void verifyFile(FileAST& fileAst);

    private:
        bool canCastType(Type* to, Type* from, bool isExplicit);
        bool typeIsAssignable(Type* checkType);

        bool checkDeclNameInUse(const std::string& name, Decl* ignoreDecl, bool ignoreFunctions = false);
        bool checkFunctionExists(FunctionDecl* function);
        bool checkParamsAreSame(std::vector<ParameterDecl*>& params1, std::vector<ParameterDecl*>& params2);

        void printError(const std::string& message, TextPosition startPosition, TextPosition endPosition);
        void printWarning(const std::string& message, TextPosition startPosition, TextPosition endPosition);
        void printDebugWarning(const std::string& message);

        void verifyDecl(Decl* decl);
        void verifyStmt(Stmt*& stmt);
        void verifyExpr(Expr*& expr);

        // Decls
        // TODO: Support verifying that a function returns on every branch
        void verifyFunctionDecl(FunctionDecl* functionDecl);
        void verifyGlobalVariableDecl(GlobalVariableDecl* globalVariableDecl);
        void verifyNamespaceDecl(NamespaceDecl* namespaceDecl);
        void verifyTemplateFunctionDecl(TemplateFunctionDecl* templateFunctionDecl);

        // Stmts
        void verifyBreakStmt(BreakStmt* breakStmt);
        void verifyCaseStmt(CaseStmt* caseStmt);
        void verifyCompoundStmt(CompoundStmt* compoundStmt);
        void verifyContinueStmt(ContinueStmt* continueStmt);
        void verifyDoStmt(DoStmt* doStmt);
        void verifyForStmt(ForStmt* forStmt);
        void verifyGotoStmt(GotoStmt* gotoStmt);
        void verifyIfStmt(IfStmt* ifStmt);
        void verifyLabeledStmt(LabeledStmt* labeledStmt);
        void verifyReturnStmt(ReturnStmt* returnStmt);
        void verifySwitchStmt(SwitchStmt* switchStmt);
        void verifyTryStmt(TryStmt* tryStmt);
        void verifyTryCatchStmt(TryCatchStmt* tryCatchStmt);
        void verifyTryFinallyStmt(TryFinallyStmt* tryFinallyStmt);
        void verifyWhileStmt(WhileStmt* whileStmt);

        // Exprs
        void verifyBinaryOperatorExpr(Expr*& expr);
        void verifyCharacterLiteralExpr(CharacterLiteralExpr* characterLiteralExpr);
        void verifyExplicitCastExpr(ExplicitCastExpr* explicitCastExpr);
        void verifyFloatLiteralExpr(FloatLiteralExpr* floatLiteralExpr);
        void verifyFunctionCallExpr(FunctionCallExpr* functionCallExpr);
        void verifyIdentifierExpr(Expr*& identifierExpr);
        void verifyImplicitCastExpr(ImplicitCastExpr* implicitCastExpr);
        void verifyIndexerCallExpr(IndexerCallExpr* indexerCallExpr);
        void verifyIntegerLiteralExpr(IntegerLiteralExpr* integerLiteralExpr);
        void verifyLocalVariableDeclExpr(LocalVariableDeclExpr* localVariableDeclExpr);
        void verifyLocalVariableDeclOrPrefixOperatorCallExpr(Expr*& expr);
        void verifyLValueToRValueExpr(LValueToRValueExpr* expr);
        void verifyMemberAccessCallExpr(MemberAccessCallExpr* memberAccessCallExpr);
        void verifyParenExpr(ParenExpr* parenExpr);
        void verifyPostfixOperatorExpr(PostfixOperatorExpr* postfixOperatorExpr);
        void verifyPotentialExplicitCastExpr(PotentialExplicitCastExpr* potentialExplicitCastExpr);
        void verifyPrefixOperatorExpr(PrefixOperatorExpr* prefixOperatorExpr);
        void verifyResolvedTypeRefExpr(ResolvedTypeRefExpr* resolvedTypeRefExpr);
        void verifyStringLiteralExpr(StringLiteralExpr* stringLiteralExpr);
        void verifyTernaryExpr(TernaryExpr* ternaryExpr);
        void verifyUnresolvedTypeRefExpr(Expr*& expr);

        // Context
        FileAST* currentFileAst;
        NamespaceDecl* currentNamespace;
        Type* currentFunctionReturnType;
        std::vector<TemplateParameterDecl*>* currentFunctionTemplateParameters;
        std::vector<ParameterDecl*>* currentFunctionParameters;
        unsigned int currentFunctionLocalVariablesCount;
        std::vector<LocalVariableDeclExpr*> currentFunctionLocalVariables;

        bool localVariableNameTaken(const std::string& varName) const {
            for (std::size_t i = 0; i < currentFunctionLocalVariablesCount; ++i) {
                if (currentFunctionLocalVariables[i]->name() == varName) {
                    return true;
                }
            }

            if (currentFunctionParameters != nullptr) {
                for (const ParameterDecl *param : *currentFunctionParameters) {
                    if (param->name() == varName) {
                        return true;
                    }
                }
            }

            if (currentFunctionTemplateParameters != nullptr) {
                for (const TemplateParameterDecl *templateParam : *currentFunctionTemplateParameters) {
                    if (templateParam->name() == varName) {
                        return true;
                    }
                }
            }

            return false;
        }

        void addLocalVariable(LocalVariableDeclExpr* localVariableDeclExpr) {
            ++currentFunctionLocalVariablesCount;

            if (currentFunctionLocalVariablesCount >= currentFunctionLocalVariables.size()) {
                currentFunctionLocalVariables.push_back(localVariableDeclExpr);
            } else {
                currentFunctionLocalVariables[currentFunctionLocalVariablesCount - 1] = localVariableDeclExpr;
            }
        }

    };
}

#endif //GULC_CODEVERIFIER_HPP
