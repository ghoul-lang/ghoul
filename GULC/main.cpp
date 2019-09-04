#include <Parser/Parser.hpp>
#include <iostream>
#include <AST/Decls/FunctionDecl.hpp>
#include <Utilities/ASTPrinter.hpp>

using namespace gulc;

int main() {
    Parser parser("Examples/FunctionTest.gul");
    Decl* decl = parser.parseTopLevelDecl();
    ASTPrinter::printDecl(decl);
    delete decl;

    decl = parser.parseTopLevelDecl();
    ASTPrinter::printDecl(decl);
    delete decl;
	return 0;
}
