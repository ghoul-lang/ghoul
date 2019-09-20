#ifndef GULC_MODULE_HPP
#define GULC_MODULE_HPP

#include <llvm/IR/Module.h>

namespace gulc {
    struct Module {
        std::string filePath;
        llvm::LLVMContext* llvmContext;
        llvm::Module* llvmModule;

        Module(std::string filePath, llvm::LLVMContext* llvmContext, llvm::Module* llvmModule)
                : filePath(std::move(filePath)), llvmContext(llvmContext), llvmModule(llvmModule) {}

    };
}

#endif //GULC_MODULE_HPP
