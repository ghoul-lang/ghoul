#include "CodeVerifier.hpp"

using namespace gulc;

void CodeVerifier::verifyFile(FileAST& fileAst) {
    for (Decl* decl : fileAst.topLevelDecls()) {
        verifyDecl(decl);
    }
}

void CodeVerifier::printError(const std::string &message, TextPosition startPosition, TextPosition endPosition) {
    std::cout << "gulc resolver error[" << currentFileAst->filePath() << ", "
                                     "{" << startPosition.line << ", " << startPosition.column << "} "
                                     "to {" << endPosition.line << ", " << endPosition.column << "}]: "
              << message
              << std::endl;
    std::exit(1);
}

void CodeVerifier::printWarning(const std::string &message, TextPosition startPosition, TextPosition endPosition) {
    std::cout << "gulc resolver warning[" << currentFileAst->filePath() << ", "
                                     "{" << startPosition.line << ", " << startPosition.column << "} "
                                     "to {" << endPosition.line << ", " << endPosition.column << "}]: "
              << message
              << std::endl;
}

void CodeVerifier::printDebugWarning(const std::string &message) {
#ifndef NDEBUG
    std::cout << "gulc verifier [DEBUG WARNING](" << currentFileAst->filePath() << "): " << message << std::endl;
#endif
}

void CodeVerifier::verifyDecl(Decl *decl) {
    switch (decl->getDeclKind()) {
        case Decl::Kind::Function:
            verifyFunctionDecl(llvm::dyn_cast<FunctionDecl>(decl));
            break;
        default:
            printDebugWarning("unhandled Decl in 'processDecl'!");
            break;
    }
}

void CodeVerifier::verifyFunctionDecl(FunctionDecl *functionDecl) {

}
