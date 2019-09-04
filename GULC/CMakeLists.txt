# CMakeList.txt : CMake project for GULC, include source and define
# project specific logic here.
#
cmake_minimum_required (VERSION 3.8)

include_directories(${CMAKE_CURRENT_SOURCE_DIR})

# Add source to this project's executable.
add_executable (GULC "main.cpp" "Lexer/Lexer.cpp" Parser/Parser.cpp Parser/Parser.hpp Lexer/LexerCheckpoint.hpp AST/Expr.cpp AST/Expr.hpp AST/Stmt.cpp AST/Stmt.hpp AST/Decl.cpp AST/Decl.hpp AST/Stmts/CompoundStmt.cpp AST/Stmts/CompoundStmt.hpp AST/Exprs/BinaryOperatorExpr.cpp AST/Exprs/BinaryOperatorExpr.hpp AST/Stmts/ReturnStmt.cpp AST/Stmts/ReturnStmt.hpp AST/Decls/FunctionDecl.cpp AST/Decls/FunctionDecl.hpp AST/Type.cpp AST/Type.hpp AST/Types/UnresolvedType.cpp AST/Types/UnresolvedType.hpp AST/Decls/ParameterDecl.cpp AST/Decls/ParameterDecl.hpp AST/Exprs/IdentifierExpr.cpp AST/Exprs/IdentifierExpr.hpp AST/Exprs/TernaryExpr.cpp AST/Exprs/TernaryExpr.hpp AST/Exprs/PrefixOperatorExpr.cpp AST/Exprs/PrefixOperatorExpr.hpp AST/Exprs/PostfixOperatorExpr.cpp AST/Exprs/PostfixOperatorExpr.hpp AST/Exprs/FunctionCallExpr.cpp AST/Exprs/FunctionCallExpr.hpp AST/Exprs/IndexerCallExpr.cpp AST/Exprs/IndexerCallExpr.hpp AST/Exprs/MemberAccessCallExpr.cpp AST/Exprs/MemberAccessCallExpr.hpp Utilities/ASTPrinter.cpp Utilities/ASTPrinter.hpp AST/Exprs/IntegerLiteralExpr.cpp AST/Exprs/IntegerLiteralExpr.hpp AST/Exprs/StringLiteralExpr.cpp AST/Exprs/StringLiteralExpr.hpp AST/Exprs/CharacterLiteralExpr.cpp AST/Exprs/CharacterLiteralExpr.hpp AST/Exprs/ParenExpr.cpp AST/Exprs/ParenExpr.hpp AST/Exprs/FloatLiteralExpr.cpp AST/Exprs/FloatLiteralExpr.hpp)

target_link_libraries(${PROJECT_NAME}
  LLVMCore
)

# Copy some example files into the directory. This just makes it easier for me to test everything is working, this will be removed in the future.
add_custom_command(
        TARGET GULC POST_BUILD
        COMMAND ${CMAKE_COMMAND} -E copy_directory
        ${CMAKE_CURRENT_SOURCE_DIR}/Examples/
        ${CMAKE_CURRENT_BINARY_DIR}/Examples/)
