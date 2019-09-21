#include <Parser/Parser.hpp>
#include <Passes/DeclResolver.hpp>
#include <CodeGen/CodeGen.hpp>
#include <ObjGen/ObjGen.hpp>
#include <Linker/Linker.hpp>

using namespace gulc;

int main() {
    Parser parser("Examples/FunctionTest.gul");
    FileAST fileAst(parser.parseFile());

    DeclResolver declResolver;
    declResolver.processFile(fileAst);

    CodeGen codeGen = CodeGen();
    gulc::Module module = codeGen.generate(fileAst);

    ObjGen::init();
    ObjGen objGen = ObjGen();

    ObjFile objFile = objGen.generate(module);
    gulc::Linker::link(objFile);

	return 0;
}
