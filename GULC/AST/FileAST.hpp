#ifndef GULC_FILEAST_HPP
#define GULC_FILEAST_HPP

#include <string>
#include <vector>
#include "Decl.hpp"
#include <iostream>

namespace gulc {
    class FileAST {
    public:
        explicit FileAST(std::string filePath)
                : _filePath(std::move(filePath)), _topLevelDecls() {}

		FileAST& operator=(FileAST&& other) noexcept = default;
		FileAST(FileAST&& other) noexcept = default;

        std::string filePath() const { return _filePath; }
        const std::vector<Decl*>& topLevelDecls() const { return _topLevelDecls; }
        void addTopLevelDecl(Decl* decl) { _topLevelDecls.push_back(decl); }

        virtual ~FileAST() {
            for (Decl* topLevelDecl : _topLevelDecls) {
                delete topLevelDecl;
            }
        }

    private:
        std::string _filePath;
        std::vector<Decl*> _topLevelDecls;

    };
}

#endif //GULC_FILEAST_HPP
