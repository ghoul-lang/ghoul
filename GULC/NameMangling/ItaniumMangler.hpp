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

#ifndef GULC_ITANIUMMANGLER_HPP
#define GULC_ITANIUMMANGLER_HPP

#include "ManglerBase.hpp"

namespace gulc {
    class ItaniumMangler : public ManglerBase {
    public:
        std::string mangle(FunctionDecl* functionDecl) override;
        std::string mangle(GlobalVariableDecl* globalVariableDecl) override;
        void mangle(NamespaceDecl* namespaceDecl, const std::string& prefix) override;
        void mangle(TemplateFunctionDecl* templateFunctionDecl) override;

    private:
        std::string unqualifiedName(FunctionDecl* functionDecl);
        std::string unqualifiedName(GlobalVariableDecl* globalVariableDecl);

        std::string sourceName(const std::string& s);
        std::string bareFunctionType(std::vector<ParameterDecl*>& params);
        std::string typeName(gulc::Type* type);

        std::string templateArgs(std::vector<TemplateParameterDecl*>& templateParams, std::vector<Expr*>& templateArgs);
        std::string templateArg(const Expr* expr);
        std::string exprPrimary(const Expr* expr);

    };
}

#endif //GULC_ITANIUMMANGLER_HPP
