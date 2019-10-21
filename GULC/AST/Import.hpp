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

#ifndef GULC_IMPORT_HPP
#define GULC_IMPORT_HPP

#include <vector>
#include <string>
#include <AST/Decls/NamespaceDecl.hpp>

namespace gulc {
    class Import {
    public:
        Import(std::vector<std::string> namespacePath, std::string alias)
                : pointToNamespace(nullptr), _namespacePath(std::move(namespacePath)), _alias(std::move(alias)) {}

        [[nodiscard]]
        const std::vector<std::string>& namespacePath() const { return _namespacePath; }
        [[nodiscard]]
        bool hasAlias() const { return !_alias.empty(); }
        [[nodiscard]]
        const std::string& alias() const { return _alias; }

        // Namespace the import points to, we don't own this so we don't free it...
        NamespaceDecl* pointToNamespace;

    private:
        std::vector<std::string> _namespacePath;
        std::string _alias;

    };
}

#endif //GULC_IMPORT_HPP
