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

#include "Target.hpp"

using namespace gulc;

Target::Target(Target::Arch arch, Target::OS os, Target::Env env)
        : _arch(arch), _os(os), _env(env) {
    switch (arch) {
        case Arch::x86_64:
            _sizeofPtr = 8;
            _sizeofUSize = 8;

            _alignofStruct = 8;
            break;
    }
}

Target Target::getHostTarget() {
    // TODO: At some point we should actually implement detectors for this... but for now we just do this...
    return Target(Arch::x86_64, OS::Linux, Env::GNU);
}
