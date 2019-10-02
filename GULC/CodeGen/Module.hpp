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
