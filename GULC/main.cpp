#include <Parser/Parser.hpp>
#include <Passes/DeclResolver.hpp>
#include <CodeGen/CodeGen.hpp>
#include <ObjGen/ObjGen.hpp>
#include <Linker/Linker.hpp>

using namespace gulc;

int main() {
    // Parse our file in
    Parser parser("Examples/FunctionTest.gul");
    FileAST fileAst(parser.parseFile());

    // Resolve declarations
    DeclResolver declResolver;
    declResolver.processFile(fileAst);

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
