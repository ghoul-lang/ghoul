# CMakeList.txt : CMake project for GULC, include source and define
# project specific logic here.
#
cmake_minimum_required (VERSION 3.8)

include_directories(${CMAKE_CURRENT_SOURCE_DIR})

# Add source to this project's executable.
add_executable (GULC "main.cpp" "Lexer/Lexer.cpp" Parser/Parser.cpp Parser/Parser.hpp Lexer/LexerCheckpoint.hpp)

target_link_libraries(${PROJECT_NAME}
  LLVMCore
)
