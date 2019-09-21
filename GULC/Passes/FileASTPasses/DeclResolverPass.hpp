#ifndef GULC_DECLRESOLVERPASS_HPP
#define GULC_DECLRESOLVERPASS_HPP

#include <map>

#include <Passes/FileASTPass.hpp>
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

namespace gulc {
    // Handles resolving variable calls and function calls to their absolute paths, also handles creating 'ImplicitCastExpr's
    class DeclResolverPass : public FileASTPass {
    public:
        DeclResolverPass()
                : currentFileAst(nullptr), returnType(nullptr), functionTemplateParams(nullptr),
                  functionParams(nullptr), functionCallArgs(nullptr), labelNames(),
                  functionLocalVariablesCount(0), functionLocalVariables() {}

        void processFile(FileAST& fileAst) override;

    private:
        bool resolveType(Type*& type);

        bool getTypesAreSame(const Type* type1, const Type* type2);
        bool getTypeGreaterThan(const Type* left, const Type* right);

        Type* deepCopyAndSimplifyType(const Type* type);

        void printError(const std::string& message, TextPosition startPosition, TextPosition endPosition);
        void printDebugWarning(const std::string& message);

        void processDecl(FileAST &fileAst, Decl* decl);
        void processStmt(Stmt*& stmt);
        void processExpr(Expr*& expr);

        void processFunctionDecl(FileAST &fileAst, FunctionDecl* functionDecl);

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

        void processBinaryOperatorExpr(BinaryOperatorExpr* binaryOperatorExpr);
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
        void processMemberAccessCallExpr(MemberAccessCallExpr* memberAccessCallExpr);
        void processParenExpr(ParenExpr* parenExpr);
        void processPostfixOperatorExpr(PostfixOperatorExpr* postfixOperatorExpr);
        void processPotentialExplicitCastExpr(PotentialExplicitCastExpr* potentialExplicitCastExpr);
        void processPrefixOperatorExpr(PrefixOperatorExpr* prefixOperatorExpr);
        void processResolvedTypeRefExpr(ResolvedTypeRefExpr* resolvedTypeRefExpr);
        void processStringLiteralExpr(StringLiteralExpr* stringLiteralExpr);
        void processTernaryExpr(TernaryExpr* ternaryExpr);
        void processUnresolvedTypeRefExpr(Expr*& expr);

        void convertLValueToRValue(Expr*& potentialLValue);

        // Context management
        FileAST* currentFileAst;
        Type* returnType;
        const std::vector<TemplateParameterDecl*>* functionTemplateParams;
        const std::vector<ParameterDecl*>* functionParams;
        const std::vector<Expr*>* functionCallArgs;
        // List of resolved and unresolved labels (if the boolean is true then it is resolved, else it isn't found)
        std::map<std::string, bool> labelNames;
        unsigned int functionLocalVariablesCount;
        std::vector<LocalVariableDeclExpr*> functionLocalVariables;

        void labelResolved(const std::string& labelName) {
            if (labelNames.find(labelName) != labelNames.end()) {
                labelNames[labelName] = true;
            } else {
                labelNames.insert({ labelName, true });
            }
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

#endif //GULC_DECLRESOLVERPASS_HPP
