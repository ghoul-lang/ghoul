#include <iostream>
#include <llvm/IR/LegacyPassManager.h>
#include "ObjGen.hpp"

#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Host.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetOptions.h"

void gulc::ObjGen::init() {
    // Maybe one day but this KILLS our build time having to wait for the linker to load and link megabytes of libraries we don't really need...
//    llvm::InitializeAllTargetInfos();
//    llvm::InitializeAllTargets();
//    llvm::InitializeAllTargetMCs();
//    llvm::InitializeAllAsmParsers();
//    llvm::InitializeAllAsmPrinters();
    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmParser();
    llvm::InitializeNativeTargetAsmPrinter();
}

void gulc::ObjGen::generate(gulc::Module module) {
    std::string filename = "build/objs/" + module.filePath + ".o";

    llvm::StringRef targetTriple = llvm::StringRef(LLVM_DEFAULT_TARGET_TRIPLE);
    module.llvmModule->setTargetTriple(targetTriple);

    std::string Error;
    auto target = llvm::TargetRegistry::lookupTarget(targetTriple, Error);

    if (!target) {
        std::cerr << Error << std::endl;
        std::exit(1);
    }

    std::string cpu = "generic";
    llvm::TargetOptions targetOptions;
    auto objTargetMachine = target->createTargetMachine(targetTriple, cpu, "", targetOptions, llvm::Optional<llvm::Reloc::Model>());

    module.llvmModule->setDataLayout(objTargetMachine->createDataLayout());

    std::error_code errorCode;
    llvm::raw_fd_ostream dest(filename, errorCode, llvm::sys::fs::OpenFlags::OF_None);

    if (errorCode) {
        std::cerr << "Could not open file: " << errorCode.message() << std::endl;
        std::exit(1);
    }

    llvm::legacy::PassManager pass;

    if (objTargetMachine->addPassesToEmitFile(pass, dest, nullptr, llvm::TargetMachine::CGFT_ObjectFile)) {
        std::cerr << "Target Machine can't emit a file of this type" << std::endl;
        std::exit(1);
    }

    pass.run(*module.llvmModule);
    dest.flush();
}
