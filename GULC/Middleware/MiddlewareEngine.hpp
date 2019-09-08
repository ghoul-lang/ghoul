#ifndef GULC_MIDDLEWAREENGINE_HPP
#define GULC_MIDDLEWAREENGINE_HPP

#include <AST/FileAST.hpp>
#include <Passes/FileASTPass.hpp>
#include <vector>
#include <memory>

namespace gulc {
    class MiddlewareEngine {
    public:
        MiddlewareEngine();

        void process(FileAST& fileAst);

    private:
        std::vector<std::unique_ptr<FileASTPass>> _fileAstPasses;

    };
}

#endif //GULC_MIDDLEWAREENGINE_HPP
