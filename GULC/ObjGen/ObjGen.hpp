#ifndef GULC_OBJGEN_HPP
#define GULC_OBJGEN_HPP

#include <CodeGen/Module.hpp>

namespace gulc {
    class ObjGen {
    public:
        static void init();

        void generate(gulc::Module module);
    };
}

#endif //GULC_OBJGEN_HPP
