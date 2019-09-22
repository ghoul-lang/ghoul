#ifndef GULC_CODEVERIFIER_HPP
#define GULC_CODEVERIFIER_HPP

#include <AST/FileAST.hpp>
#include <AST/Decls/FunctionDecl.hpp>
#include <AST/Exprs/LocalVariableDeclExpr.hpp>

namespace gulc {
    class CodeVerifier {
    public:
        CodeVerifier()
                : currentFileAst(nullptr) {}

        void verifyFile(FileAST& fileAst);

    private:
        void printError(const std::string& message, TextPosition startPosition, TextPosition endPosition);
        void printWarning(const std::string& message, TextPosition startPosition, TextPosition endPosition);
        void printDebugWarning(const std::string& message);

        void verifyDecl(Decl* decl);

        // Decls
        // TODO: Support verifying that a function returns on every branch
        void verifyFunctionDecl(FunctionDecl* functionDecl);

        // Stmts
        void verifyCompoundStmt(CompoundStmt* compoundStmt);

        // Context
        FileAST* currentFileAst;
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
