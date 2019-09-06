#include <Parser/Parser.hpp>
#include <Utilities/ASTPrinter.hpp>

using namespace gulc;

int main() {
    Parser parser("Examples/FunctionTest.gul");
    FileAST fileAst(parser.parseFile());

    for (const Decl* topLevelDecl : fileAst.topLevelDecls()) {
        ASTPrinter::printDecl(topLevelDecl);
    }

	return 0;
}
