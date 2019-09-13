#include "CodeGen.hpp"

using namespace gulc;

void CodeGen::generate(gulc::FileAST& file) {
    CodeGenContext codeGenContext = CodeGenContext(file);

    for (const Decl* decl : file.topLevelDecls()) {
        generateDecl(codeGenContext, decl);
    }
}

void CodeGen::printError(const std::string &message, FileAST &fileAst, TextPosition startPosition, TextPosition endPosition) {
    std::cout << "gulc codegen error[" << fileAst.filePath() << ", "
                                    "{" << startPosition.line << ", " << startPosition.column << "} "
                                    "to {" << endPosition.line << ", " << endPosition.column << "}]: "
              << message
              << std::endl;
    std::exit(1);
}

void CodeGen::generateDecl(CodeGenContext& context, const Decl *decl) {
    switch (decl->getDeclKind()) {
        case Decl::Kind::Function:
            return generateFunctionDecl(context, llvm::dyn_cast<FunctionDecl>(decl));
        default:
            printError("internal - unsupported decl!",
                       context.fileAst, decl->startPosition(), decl->endPosition());
            break;
    }
}

void CodeGen::generateFunctionDecl(CodeGenContext& context, const FunctionDecl *functionDecl) {

}
