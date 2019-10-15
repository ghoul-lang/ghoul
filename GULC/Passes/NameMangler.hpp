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

#ifndef GULC_NAMEMANGLER_HPP
#define GULC_NAMEMANGLER_HPP

#include <AST/FileAST.hpp>
#include <NameMangling/ManglerBase.hpp>
#include <AST/Decls/NamespaceDecl.hpp>

namespace gulc {
    class NameMangler {
    public:
        explicit NameMangler(ManglerBase* manglerBase)
                : _manglerBase(manglerBase) {}

        void processFile(FileAST& fileAst);

    private:
        ManglerBase* _manglerBase;

        void processDecl(Decl* decl);

    };
}

#endif //GULC_NAMEMANGLER_HPP
