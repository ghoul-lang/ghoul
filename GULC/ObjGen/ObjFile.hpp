#ifndef GULC_OBJFILE_HPP
#define GULC_OBJFILE_HPP

#include <string>

namespace gulc {
    struct ObjFile {
        std::string filePath;

        explicit ObjFile(std::string filePath) : filePath(std::move(filePath)) {}
    };
}

#endif //GULC_OBJFILE_HPP
