#include <iostream>
#include "main.hpp"
#include "llvm/IR/Module.h"

int main()
{
#ifdef __clang__
    std::cout << "Hello Clang." << std::endl;
#endif

	return 0;
}
