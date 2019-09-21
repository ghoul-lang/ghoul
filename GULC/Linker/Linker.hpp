#ifndef GULC_LINKER_HPP
#define GULC_LINKER_HPP

#include <CodeGen/Module.hpp>
#include <ObjGen/ObjFile.hpp>

namespace gulc {
    class Linker {
    public:
        static void link(ObjFile objFile);
    };
}

#endif //GULC_LINKER_HPP
