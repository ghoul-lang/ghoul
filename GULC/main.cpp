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

#include <Parser/Parser.hpp>
#include <Passes/DeclResolver.hpp>
#include <CodeGen/CodeGen.hpp>
#include <ObjGen/ObjGen.hpp>
#include <Linker/Linker.hpp>
#include <Passes/CodeVerifier.hpp>
#include <Passes/TypeResolver.hpp>
#include <Passes/NameMangler.hpp>
#include <NameMangling/ItaniumMangler.hpp>
#include <Passes/NamespacePrototyper.hpp>

using namespace gulc;

int main() {
    // Parse our file in
    Parser parser("Examples/FunctionTest.gul");
    FileAST fileAst(parser.parseFile());

    // Generate namespace map
    NamespacePrototyper namespacePrototyper;
    std::vector<NamespaceDecl*> prototypes = namespacePrototyper.generatePrototypes(fileAst);

    // Resolve types
    TypeResolver typeResolver(prototypes);
    typeResolver.processFile(fileAst);

    // Resolve declarations
    DeclResolver declResolver;
    declResolver.processFile(fileAst);

    // Mangle names (for overloading support)
    NameMangler nameMangler(new ItaniumMangler());
    nameMangler.processFile(fileAst);

    // Translate operations and verify operations can be performed
    CodeVerifier codeVerifier;
    codeVerifier.verifyFile(fileAst);

    // Generate the LLVM IR
    CodeGen codeGen = CodeGen();
    gulc::Module module = codeGen.generate(fileAst);

    // Generate the object files
    ObjGen::init();
    ObjGen objGen = ObjGen();
    ObjFile objFile = objGen.generate(module);

    // Link the object files together
    gulc::Linker::link(objFile);

	return 0;
}
