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

#include "Linker.hpp"

void gulc::Linker::link(std::vector<ObjFile>& objFiles) {
    // Create the entry object...
    std::string asmArgs = "as Examples/entry.s -o build/objs/Examples/entry.o";
    std::system(asmArgs.c_str());

    std::string objFilesPath;

    for (ObjFile& objFile : objFiles) {
        objFilesPath += " " + objFile.filePath;
    }

    // Link everything together...
    std::string linkerArgs = "ld build/objs/Examples/entry.o " + objFilesPath + " -o a.out";
    std::system(linkerArgs.c_str());
}
