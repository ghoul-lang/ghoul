#ifndef GULC_OBJGEN_HPP
#define GULC_OBJGEN_HPP

#include <CodeGen/Module.hpp>
#include "ObjFile.hpp"

namespace gulc {
    class ObjGen {
    public:
        static void init();

        ObjFile generate(gulc::Module module);
    };
}

#endif //GULC_OBJGEN_HPP
