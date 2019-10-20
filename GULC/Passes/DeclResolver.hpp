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

#ifndef GULC_DECLRESOLVER_HPP
#define GULC_DECLRESOLVER_HPP

#include <map>

#include <AST/Decls/TemplateParameterDecl.hpp>
#include <AST/Decls/FunctionDecl.hpp>
#include <AST/Decls/ParameterDecl.hpp>
#include <AST/Stmts/DoStmt.hpp>
#include <AST/Stmts/ForStmt.hpp>
#include <AST/Stmts/IfStmt.hpp>
#include <AST/Stmts/LabeledStmt.hpp>
#include <AST/Stmts/SwitchStmt.hpp>
#include <AST/Stmts/TryStmt.hpp>
#include <AST/Stmts/TryCatchStmt.hpp>
#include <AST/Stmts/WhileStmt.hpp>
#include <AST/Stmts/ReturnStmt.hpp>
#include <AST/Exprs/BinaryOperatorExpr.hpp>
#include <AST/Exprs/FunctionCallExpr.hpp>
#include <AST/Exprs/IdentifierExpr.hpp>
#include <AST/Exprs/IndexerCallExpr.hpp>
#include <AST/Exprs/MemberAccessCallExpr.hpp>
#include <AST/Exprs/ParenExpr.hpp>
#include <AST/Exprs/PostfixOperatorExpr.hpp>
#include <AST/Exprs/PotentialExplicitCastExpr.hpp>
#include <AST/Exprs/PrefixOperatorExpr.hpp>
#include <AST/Exprs/TernaryExpr.hpp>
#include <AST/Stmts/BreakStmt.hpp>
#include <AST/Stmts/ContinueStmt.hpp>
#include <AST/Stmts/GotoStmt.hpp>
#include <AST/Exprs/CharacterLiteralExpr.hpp>
#include <AST/Exprs/ExplicitCastExpr.hpp>
#include <AST/Exprs/FloatLiteralExpr.hpp>
#include <AST/Exprs/ImplicitCastExpr.hpp>
#include <AST/Exprs/IntegerLiteralExpr.hpp>
#include <AST/Exprs/LocalVariableDeclExpr.hpp>
#include <AST/Exprs/ResolvedTypeRefExpr.hpp>
#include <AST/Exprs/StringLiteralExpr.hpp>
#include <AST/Exprs/LocalVariableDeclExpr.hpp>
#include <AST/FileAST.hpp>
#include <AST/Decls/GlobalVariableDecl.hpp>
#include <AST/Decls/EnumDecl.hpp>
#include <AST/Decls/NamespaceDecl.hpp>
#include <AST/Decls/TemplateFunctionDecl.hpp>

namespace gulc {
    // Handles resolving variable calls and function calls to their absolute paths, also handles creating 'ImplicitCastExpr's
    class DeclResolver {
        friend class CodeVerifier;

    public:
        DeclResolver()
                : currentFileAst(nullptr), currentNamespace(nullptr), returnType(nullptr),
                  functionTemplateParams(nullptr), functionTemplateArgs(nullptr), functionParams(nullptr),
                  exprIsFunctionCall(false), functionCallArgs(nullptr), labelNames(), functionLocalVariablesCount(0),
                  functionLocalVariables() {}

        void processFile(FileAST& fileAst);

        static Expr* solveConstExpression(Expr* expr);

    private:
        bool getTypeIsReference(const Type* check);
        bool canImplicitCast(const Type* to, const Type* from);

        /**
         * Checks if the result types of `args` matches the input types of `params`
         * If the result type of an `arg` requires an implicit cast to match `param` then `isExact` is set to false
         */
        bool argsMatchParams(const std::vector<ParameterDecl*>& params, const std::vector<Expr*>* args, bool* isExact,
                             const std::vector<TemplateParameterDecl*>& functionCallTemplateParams = {},
                             const std::vector<Expr*>& functionCallTemplateArgs = {});
        bool templateArgsMatchParams(const std::vector<TemplateParameterDecl*>& params, const std::vector<Expr*>& args);
        /// Returns false on error
        bool checkFunctionMatchesCall(FunctionDecl*& currentFoundFunction, FunctionDecl* checkFunction,
                                      bool* isExactMatch, bool* isAmbiguous);
        bool checkTemplateFunctionMatchesCall(FunctionDecl*& currentFoundFunction, TemplateFunctionDecl* checkFunction,
                                              bool* isExactMatch, bool* isAmbiguous,
                                              std::vector<Expr*>& functionCallTemplateArgs);

        void printError(const std::string& message, TextPosition startPosition, TextPosition endPosition);
        void printWarning(const std::string& message, TextPosition startPosition, TextPosition endPosition);
        void printDebugWarning(const std::string& message);

        void processDecl(Decl* decl);
        void processStmt(Stmt*& stmt);
        void processExpr(Expr*& expr);

        void processEnumDecl(EnumDecl* enumDecl);
        void processFunctionDecl(FunctionDecl* functionDecl);
        void processGlobalVariableDecl(GlobalVariableDecl* globalVariableDecl);
        void processNamespaceDecl(NamespaceDecl* namespaceDecl);
        void processTemplateFunctionDecl(TemplateFunctionDecl* templateFunctionDecl);
        void processTemplateFunctionDeclImplementation(TemplateFunctionDecl* templateFunctionDecl,
                                                       std::vector<Expr*>& templateArgs,
                                                       FunctionDecl* implementedFunction);

        void processBreakStmt(BreakStmt* breakStmt);
        void processCaseStmt(CaseStmt* caseStmt);
        void processCompoundStmt(CompoundStmt* compoundStmt);
        void processContinueStmt(ContinueStmt* continueStmt);
        void processDoStmt(DoStmt* doStmt);
        void processForStmt(ForStmt* forStmt);
        void processGotoStmt(GotoStmt* gotoStmt);
        void processIfStmt(IfStmt* ifStmt);
        void processLabeledStmt(LabeledStmt* labeledStmt);
        void processReturnStmt(ReturnStmt* returnStmt);
        void processSwitchStmt(SwitchStmt* switchStmt);
        void processTryStmt(TryStmt* tryStmt);
        void processTryCatchStmt(TryCatchStmt* tryCatchStmt);
        void processTryFinallyStmt(TryFinallyStmt* tryFinallyStmt);
        void processWhileStmt(WhileStmt* whileStmt);

        void processBinaryOperatorExpr(Expr*& expr);
        void processCharacterLiteralExpr(CharacterLiteralExpr* characterLiteralExpr);
        void processExplicitCastExpr(ExplicitCastExpr* explicitCastExpr);
        void processFloatLiteralExpr(FloatLiteralExpr* floatLiteralExpr);
        void processFunctionCallExpr(FunctionCallExpr* functionCallExpr);
        void processIdentifierExpr(Expr*& identifierExpr);
        void processImplicitCastExpr(ImplicitCastExpr* implicitCastExpr);
        void processIndexerCallExpr(IndexerCallExpr* indexerCallExpr);
        void processIntegerLiteralExpr(IntegerLiteralExpr* integerLiteralExpr);
        void processLocalVariableDeclExpr(LocalVariableDeclExpr* localVariableDeclExpr);
        void processLocalVariableDeclOrPrefixOperatorCallExpr(Expr*& expr);
        void processMemberAccessCallExpr(Expr*& expr);
        void processParenExpr(ParenExpr* parenExpr);
        void processPostfixOperatorExpr(PostfixOperatorExpr* postfixOperatorExpr);
        void processPotentialExplicitCastExpr(Expr*& expr);
        void processPrefixOperatorExpr(PrefixOperatorExpr* prefixOperatorExpr);
        void processResolvedTypeRefExpr(ResolvedTypeRefExpr* resolvedTypeRefExpr);
        void processStringLiteralExpr(StringLiteralExpr* stringLiteralExpr);
        void processTernaryExpr(TernaryExpr* ternaryExpr);
        void processUnresolvedTypeRefExpr(Expr*& expr);

        void dereferenceReferences(Expr*& potentialReference);
        void convertLValueToRValue(Expr*& potentialLValue);

        /// Changes the type to an absolute type if the type is a template type
        /// Returns template parameter number for the type
        unsigned int applyTemplateTypeArguments(Type*& potentialTemplateType);

        // Context management
        FileAST* currentFileAst;
        NamespaceDecl* currentNamespace;
        Type* returnType;
        const std::vector<TemplateParameterDecl*>* functionTemplateParams;
        const std::vector<Expr*>* functionTemplateArgs;
        const std::vector<ParameterDecl*>* functionParams;
        // This is used to tell `processIdentifierExpr` to enable searching functions.
        bool exprIsFunctionCall;
        const std::vector<Expr*>* functionCallArgs;
        // List of resolved and unresolved labels (if the boolean is true then it is resolved, else it isn't found)
        std::map<std::string, bool> labelNames;
        unsigned int functionLocalVariablesCount;
        std::vector<LocalVariableDeclExpr*> functionLocalVariables;

        void labelResolved(const std::string& labelName) {
//            if (labelNames.find(labelName) != labelNames.end()) {
//                labelNames[labelName] = true;
//            } else {
//                labelNames.insert({ labelName, true });
//            }
            labelNames.insert_or_assign(labelName, true);
        }

        void addUnresolvedLabel(const std::string& labelName) {
            if (labelNames.find(labelName) == labelNames.end()) {
                labelNames.insert({ labelName, false });
            }
        }

        bool localVariableNameTaken(const std::string& varName) const {
            for (std::size_t i = 0; i < functionLocalVariablesCount; ++i) {
                if (functionLocalVariables[i]->name() == varName) {
                    return true;
                }
            }

            if (functionParams != nullptr) {
                for (const ParameterDecl *param : *functionParams) {
                    if (param->name() == varName) {
                        return true;
                    }
                }
            }

            if (functionTemplateParams != nullptr) {
                for (const TemplateParameterDecl *templateParam : *functionTemplateParams) {
                    if (templateParam->name() == varName) {
                        return true;
                    }
                }
            }

            return false;
        }

        void addLocalVariable(LocalVariableDeclExpr* localVariableDeclExpr) {
            ++functionLocalVariablesCount;

            if (functionLocalVariablesCount >= functionLocalVariables.size()) {
                functionLocalVariables.push_back(localVariableDeclExpr);
            } else {
                functionLocalVariables[functionLocalVariablesCount - 1] = localVariableDeclExpr;
            }
        }
    };
}

#endif //GULC_DECLRESOLVER_HPP
