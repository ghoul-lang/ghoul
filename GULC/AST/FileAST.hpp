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

#ifndef GULC_FILEAST_HPP
#define GULC_FILEAST_HPP

#include <algorithm>
#include <string>
#include <vector>
#include "Decl.hpp"
#include "Import.hpp"
#include <iostream>

namespace gulc {
    class FileAST {
    public:
        explicit FileAST(std::string filePath)
                : _filePath(std::move(filePath)), _imports(), _importExterns(), _topLevelDecls() {}

		FileAST& operator=(FileAST&& other) noexcept = default;
		FileAST(FileAST&& other) noexcept = default;

        std::string filePath() const { return _filePath; }
        const std::vector<Decl*>& topLevelDecls() const { return _topLevelDecls; }
        void addTopLevelDecl(Decl* decl) { _topLevelDecls.push_back(decl); }

        // Import statements/declarations (not sure exactly what they would be considered... more likely static statements?)
        std::vector<Import*>& imports() { return _imports; }
        const std::vector<Import*>& imports() const { return _imports; }
        void addImport(Import* anImport) { _imports.push_back(anImport); }

        // Import externs are `Decl` classes that we create `extern` prototypes to in our object file
        std::vector<const Decl*>& importExterns() { return _importExterns; };
        const std::vector<const Decl*>& importExterns() const { return _importExterns; };

        void addImportExtern(const Decl* decl) {
            if (std::find(_importExterns.begin(), _importExterns.end(), decl) == _importExterns.end()) {
                _importExterns.push_back(decl);
            }
        }

        virtual ~FileAST() {
            for (Decl* topLevelDecl : _topLevelDecls) {
                delete topLevelDecl;
            }
        }

    private:
        std::string _filePath;
        // These are our actual `import`s `import std.io`, `import std.gui.graphics as gfx`
        std::vector<Import*> _imports;
        // We don't own these so we don't delete them.
        std::vector<const Decl*> _importExterns;
        std::vector<Decl*> _topLevelDecls;

    };
}

#endif //GULC_FILEAST_HPP
