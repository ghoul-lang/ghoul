#include <Parser/Parser.hpp>
#include <Middleware/MiddlewareEngine.hpp>
#include <CodeGen/CodeGen.hpp>
#include <ObjGen/ObjGen.hpp>
#include <Linker/Linker.hpp>

using namespace gulc;

int main() {
    MiddlewareEngine middlewareEngine;

    Parser parser("Examples/FunctionTest.gul");
    FileAST fileAst(parser.parseFile());

    middlewareEngine.process(fileAst);

    CodeGen codeGen = CodeGen();
    gulc::Module module = codeGen.generate(fileAst);

    ObjGen::init();
    ObjGen objGen = ObjGen();

    ObjFile objFile = objGen.generate(module);
    gulc::Linker::link(objFile);

	return 0;
}
