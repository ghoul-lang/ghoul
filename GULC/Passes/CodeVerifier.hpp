#ifndef GULC_CODEVERIFIER_HPP
#define GULC_CODEVERIFIER_HPP

#include <AST/FileAST.hpp>
#include <AST/Decls/FunctionDecl.hpp>

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
        void verifyFunctionDecl(FunctionDecl* functionDecl);

        // Context
        FileAST* currentFileAst;

    };
}

#endif //GULC_CODEVERIFIER_HPP
