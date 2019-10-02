// Copyright (C) 2019 Michael Brandon Huddle
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as published
// by the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public License
// along with this program.  If not, see <https://www.gnu.org/licenses/>.

#include <iostream>
#ifdef __GNUC__
#include <experimental/filesystem>
#else
#include <filesystem>
#endif
#include <llvm/IR/LegacyPassManager.h>
#include "ObjGen.hpp"

#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Host.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetOptions.h"

#ifdef __GNUC__
namespace std_fs = std::experimental::filesystem;
#else
namespace std_fs = std::filesystem;
#endif

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

gulc::ObjFile gulc::ObjGen::generate(gulc::Module module) {
    std::string filename = "build/objs/" + module.filePath + ".o";

    // Check to see if the filename's directory exists, if it doesn't we create the directories...
    {
        std_fs::path objFilePath = filename;
        std_fs::path parentDir = objFilePath.parent_path();

        if (!std_fs::exists(parentDir)) {
            std_fs::create_directories(parentDir);
        }
    }

    std::string targetTriple = llvm::sys::getDefaultTargetTriple();
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
    llvm::raw_fd_ostream dest(filename, errorCode, llvm::sys::fs::OpenFlags::F_None);

    if (errorCode) {
        std::cerr << "Could not open file: " << errorCode.message() << std::endl;
        std::exit(1);
    }

    llvm::legacy::PassManager pass;

    if (objTargetMachine->addPassesToEmitFile(pass, dest, llvm::TargetMachine::CGFT_ObjectFile)) {
        std::cerr << "Target Machine can't emit a file of this type" << std::endl;
        std::exit(1);
    }

    pass.run(*module.llvmModule);
    dest.flush();

    return gulc::ObjFile(filename);
}
