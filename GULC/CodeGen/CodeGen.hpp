#ifndef GULC_CODEGEN_HPP
#define GULC_CODEGEN_HPP

#include <AST/FileAST.hpp>
#include <AST/Decls/FunctionDecl.hpp>

namespace gulc {
    struct CodeGenContext {
        FileAST& fileAst;

        explicit CodeGenContext(FileAST& fileAst) : fileAst(fileAst) {}
    };

    class CodeGen {
    public:
        void generate(FileAST& file);

    private:
        void printError(const std::string& message, FileAST &fileAst, TextPosition startPosition, TextPosition endPosition);

        void generateDecl(CodeGenContext& context, const Decl* decl);
        void generateFunctionDecl(CodeGenContext& context, const FunctionDecl* functionDecl);

    };
}

#endif //GULC_CODEGEN_HPP
