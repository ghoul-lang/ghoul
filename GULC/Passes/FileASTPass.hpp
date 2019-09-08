#ifndef GULC_FILEASTPASS_HPP
#define GULC_FILEASTPASS_HPP

#include <AST/FileAST.hpp>
#include "PassBase.hpp"

namespace gulc {
    class FileASTPass : public PassBase {
    public:
        virtual void processFile(FileAST& fileAst) = 0;

    };
}

#endif //GULC_FILEASTPASS_HPP
