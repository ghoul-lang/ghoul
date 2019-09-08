#include <Passes/FileASTPasses/TypeResolverPass.hpp>
#include "MiddlewareEngine.hpp"

gulc::MiddlewareEngine::MiddlewareEngine()
        : _fileAstPasses() {
    _fileAstPasses.push_back(std::unique_ptr<FileASTPass>(new TypeResolverPass()));
}

void gulc::MiddlewareEngine::process(gulc::FileAST &fileAst) {
    for (const std::unique_ptr<FileASTPass>& fileAstPass : _fileAstPasses) {
        fileAstPass->processFile(fileAst);
    }
}
