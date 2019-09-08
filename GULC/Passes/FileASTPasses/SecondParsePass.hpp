#ifndef GULC_SECONDPARSEPASS_HPP
#define GULC_SECONDPARSEPASS_HPP

#include <Passes/FileASTPass.hpp>

namespace gulc {
    // I couldn't think of a better name for this. This pass is a second stage of the 'Parser' that will take ambiguous 'Expr's and convert them to solid 'Expr's
    class SecondParsePass : public FileASTPass {
    public:
        void processFile(FileAST& fileAst) override;

    };
}

#endif //GULC_SECONDPARSEPASS_HPP
