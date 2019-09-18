#include <Parser/Parser.hpp>
#include <Utilities/ASTPrinter.hpp>
#include <Middleware/MiddlewareEngine.hpp>
#include <CodeGen/CodeGen.hpp>

using namespace gulc;

int main() {
    MiddlewareEngine middlewareEngine;

    Parser parser("Examples/FunctionTest.gul");
    FileAST fileAst(parser.parseFile());

    middlewareEngine.process(fileAst);

//    for (const Decl* topLevelDecl : fileAst.topLevelDecls()) {
//        ASTPrinter::printDecl(topLevelDecl);
//    }

    CodeGen codeGen = CodeGen();
    codeGen.generate(fileAst);

	return 0;
}
