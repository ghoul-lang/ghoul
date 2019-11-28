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

#ifndef GULC_TARGET_HPP
#define GULC_TARGET_HPP

#include <cstddef>

namespace gulc {
    class Target {
    public:
        // Currently only support x86_64
        enum class Arch {
            x86_64
        };

        enum class OS {
            Linux,
            // TODO: Fix the weird LLVM bug found on Windows caused by the LLVM Triple string destructor being weird,
            //       (bug seems to be out of our hands, seems to be a bug within the Windows C++ library?)
            Windows
        };

        enum class Env {
            GNU,
            MSVC
        };

    private:
        Arch _arch;
        OS _os;
        Env _env;

        std::size_t _sizeofPtr;
        /// sizeof(usize) == sizeof(isize)
        std::size_t _sizeofUSize;

        /// The align of for struct also tells us how to pad the struct
        std::size_t _alignofStruct;

    public:
        Target(Arch arch, OS os, Env env);

        [[nodiscard]]
        Arch getArch() const { return _arch; }
        [[nodiscard]]
        OS getOS() const { return _os; }
        [[nodiscard]]
        Env getEnv() const { return _env; }

        [[nodiscard]]
        std::size_t sizeofPtr() const { return _sizeofPtr; }
        [[nodiscard]]
        std::size_t sizeofUSize() const { return _sizeofUSize; }
        [[nodiscard]]
        std::size_t sizeofISize() const { return _sizeofUSize; }

        [[nodiscard]]
        std::size_t alignofStruct() const { return _alignofStruct; }

        static Target getHostTarget();

    };
}

#endif //GULC_TARGET_HPP
