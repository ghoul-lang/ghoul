#include "Linker.hpp"

void gulc::Linker::link(ObjFile objFile) {
    // Create the entry object...
    std::string asmArgs = "as Examples/entry.s -o build/objs/Examples/entry.o";
    std::system(asmArgs.c_str());

    // Link everything together...
    std::string linkerArgs = "ld build/objs/Examples/entry.o " + objFile.filePath + " -o a.out";
    std::system(linkerArgs.c_str());
}
