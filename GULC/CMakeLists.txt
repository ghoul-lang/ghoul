﻿# CMakeList.txt : CMake project for GULC, include source and define
# project specific logic here.
#
cmake_minimum_required (VERSION 3.8)

set (CMAKE_CXX_STANDARD 17)

include_directories(${CMAKE_CURRENT_SOURCE_DIR})

# Add source to this project's executable.
add_executable(gulc main.cpp Lexer/Lexer.cpp Parser/Parser.cpp Parser/Parser.hpp Lexer/LexerCheckpoint.hpp AST/Expr.cpp AST/Expr.hpp AST/Stmt.cpp AST/Stmt.hpp AST/Decl.cpp AST/Decl.hpp AST/Stmts/CompoundStmt.cpp AST/Stmts/CompoundStmt.hpp AST/Exprs/BinaryOperatorExpr.cpp AST/Exprs/BinaryOperatorExpr.hpp AST/Stmts/ReturnStmt.cpp AST/Stmts/ReturnStmt.hpp AST/Decls/FunctionDecl.cpp AST/Decls/FunctionDecl.hpp AST/Type.cpp AST/Type.hpp AST/Types/UnresolvedType.cpp AST/Types/UnresolvedType.hpp AST/Decls/ParameterDecl.cpp AST/Decls/ParameterDecl.hpp AST/Exprs/IdentifierExpr.cpp AST/Exprs/IdentifierExpr.hpp AST/Exprs/TernaryExpr.cpp AST/Exprs/TernaryExpr.hpp AST/Exprs/PrefixOperatorExpr.cpp AST/Exprs/PrefixOperatorExpr.hpp AST/Exprs/PostfixOperatorExpr.cpp AST/Exprs/PostfixOperatorExpr.hpp AST/Exprs/FunctionCallExpr.cpp AST/Exprs/FunctionCallExpr.hpp AST/Exprs/IndexerCallExpr.cpp AST/Exprs/IndexerCallExpr.hpp AST/Exprs/MemberAccessCallExpr.cpp AST/Exprs/MemberAccessCallExpr.hpp Utilities/ASTPrinter.cpp Utilities/ASTPrinter.hpp AST/Exprs/IntegerLiteralExpr.cpp AST/Exprs/IntegerLiteralExpr.hpp AST/Exprs/StringLiteralExpr.cpp AST/Exprs/StringLiteralExpr.hpp AST/Exprs/CharacterLiteralExpr.cpp AST/Exprs/CharacterLiteralExpr.hpp AST/Exprs/ParenExpr.cpp AST/Exprs/ParenExpr.hpp AST/Exprs/FloatLiteralExpr.cpp AST/Exprs/FloatLiteralExpr.hpp AST/Exprs/PotentialExplicitCastExpr.cpp AST/Exprs/PotentialExplicitCastExpr.hpp AST/Decls/TemplateParameterDecl.cpp AST/Decls/TemplateParameterDecl.hpp AST/Types/TemplateTypenameType.cpp AST/Types/TemplateTypenameType.hpp AST/Exprs/PotentialLocalVariableDeclExpr.cpp AST/Exprs/PotentialLocalVariableDeclExpr.hpp AST/Stmts/LabeledStmt.cpp AST/Stmts/LabeledStmt.hpp AST/Stmts/IfStmt.cpp AST/Stmts/IfStmt.hpp AST/Stmts/WhileStmt.cpp AST/Stmts/WhileStmt.hpp AST/Stmts/ForStmt.cpp AST/Stmts/ForStmt.hpp AST/Stmts/DoStmt.cpp AST/Stmts/DoStmt.hpp AST/Stmts/SwitchStmt.cpp AST/Stmts/SwitchStmt.hpp AST/Stmts/CaseStmt.cpp AST/Stmts/CaseStmt.hpp AST/Stmts/BreakStmt.cpp AST/Stmts/BreakStmt.hpp AST/Stmts/ContinueStmt.cpp AST/Stmts/ContinueStmt.hpp AST/Stmts/GotoStmt.cpp AST/Stmts/GotoStmt.hpp AST/Stmts/TryStmt.cpp AST/Stmts/TryStmt.hpp AST/Stmts/TryCatchStmt.cpp AST/Stmts/TryCatchStmt.hpp AST/Stmts/TryFinallyStmt.cpp AST/Stmts/TryFinallyStmt.hpp AST/FileAST.cpp AST/FileAST.hpp MetaData/TextPosition.cpp AST/Types/FunctionTemplateTypenameRefType.cpp AST/Types/FunctionTemplateTypenameRefType.hpp AST/Exprs/ResolvedTypeRefExpr.cpp AST/Exprs/ResolvedTypeRefExpr.hpp AST/Exprs/LocalVariableDeclExpr.cpp AST/Exprs/LocalVariableDeclExpr.hpp AST/Types/BuiltInType.cpp AST/Types/BuiltInType.hpp AST/Exprs/UnresolvedTypeRefExpr.cpp AST/Exprs/UnresolvedTypeRefExpr.hpp Passes/DeclResolver.cpp Passes/DeclResolver.hpp AST/Exprs/ExplicitCastExpr.cpp AST/Exprs/ExplicitCastExpr.hpp AST/Exprs/ImplicitCastExpr.cpp AST/Exprs/ImplicitCastExpr.hpp AST/Types/PointerType.cpp AST/Types/PointerType.hpp AST/Types/FunctionPointerType.cpp AST/Types/FunctionPointerType.hpp CodeGen/CodeGen.cpp CodeGen/CodeGen.hpp AST/Exprs/LValueToRValueExpr.cpp AST/Exprs/LValueToRValueExpr.hpp ObjGen/ObjGen.cpp ObjGen/ObjGen.hpp CodeGen/Module.cpp CodeGen/Module.hpp Linker/Linker.cpp Linker/Linker.hpp ObjGen/ObjFile.cpp ObjGen/ObjFile.hpp Passes/CodeVerifier.cpp Passes/CodeVerifier.hpp AST/Types/ReferenceType.cpp AST/Types/ReferenceType.hpp AST/Exprs/RefLocalVariableExpr.cpp AST/Exprs/RefLocalVariableExpr.hpp AST/Exprs/RefParameterExpr.cpp AST/Exprs/RefParameterExpr.hpp AST/Exprs/RefFunctionExpr.cpp AST/Exprs/RefFunctionExpr.hpp AST/Decls/GlobalVariableDecl.cpp AST/Decls/GlobalVariableDecl.hpp AST/Exprs/RefGlobalVariableExpr.cpp AST/Exprs/RefGlobalVariableExpr.hpp AST/Decls/EnumDecl.cpp AST/Decls/EnumDecl.hpp AST/Decls/EnumConstantDecl.cpp AST/Decls/EnumConstantDecl.hpp AST/Types/EnumType.cpp AST/Types/EnumType.hpp Passes/TypeResolver.cpp Passes/TypeResolver.hpp NameMangling/ManglerBase.cpp NameMangling/ManglerBase.hpp NameMangling/ItaniumMangler.cpp NameMangling/ItaniumMangler.hpp Passes/NameMangler.cpp Passes/NameMangler.hpp AST/Exprs/RefEnumConstantExpr.cpp AST/Exprs/RefEnumConstantExpr.hpp AST/Decls/NamespaceDecl.cpp AST/Decls/NamespaceDecl.hpp Passes/NamespacePrototyper.cpp Passes/NamespacePrototyper.hpp AST/Exprs/TempNamespaceRefExpr.cpp AST/Exprs/TempNamespaceRefExpr.hpp AST/Decls/TemplateFunctionDecl.cpp AST/Decls/TemplateFunctionDecl.hpp ASTHelpers/TypeComparer.cpp ASTHelpers/TypeComparer.hpp AST/Import.cpp AST/Import.hpp AST/Decls/StructDecl.cpp AST/Decls/StructDecl.hpp AST/Types/StructType.cpp AST/Types/StructType.hpp AST/Exprs/RefStructMemberVariableExpr.cpp AST/Exprs/RefStructMemberVariableExpr.hpp AST/Exprs/RefStructMemberFunctionExpr.cpp AST/Exprs/RefStructMemberFunctionExpr.hpp AST/Decls/ConstructorDecl.cpp AST/Decls/ConstructorDecl.hpp Passes/Lifetimes.cpp Passes/Lifetimes.hpp AST/Exprs/DestructLocalVariableExpr.cpp AST/Exprs/DestructLocalVariableExpr.hpp AST/Decls/DestructorDecl.cpp AST/Decls/DestructorDecl.hpp AST/Exprs/DestructParameterExpr.cpp AST/Exprs/DestructParameterExpr.hpp AST/Exprs/DestructMemberVariableExpr.cpp AST/Exprs/DestructMemberVariableExpr.hpp ASTHelpers/VisibilityChecker.cpp ASTHelpers/VisibilityChecker.hpp AST/Exprs/BaseConstructorCallExpr.cpp AST/Exprs/BaseConstructorCallExpr.hpp ASTHelpers/FunctionComparer.cpp ASTHelpers/FunctionComparer.hpp Passes/Inheriter.cpp Passes/Inheriter.hpp AST/Exprs/BaseDestructorCallExpr.cpp AST/Exprs/BaseDestructorCallExpr.hpp AST/Exprs/RefBaseExpr.cpp AST/Exprs/RefBaseExpr.hpp Targets/Target.cpp Targets/Target.hpp ASTHelpers/SizeofHelper.cpp ASTHelpers/SizeofHelper.hpp make_reverse_iterator.cpp make_reverse_iterator.hpp AST/Types/FlatArrayType.cpp AST/Types/FlatArrayType.hpp AST/Types/VTableType.cpp AST/Types/VTableType.hpp AST/Stmts/ConstructStructMemberVariableStmt.cpp AST/Stmts/ConstructStructMemberVariableStmt.hpp Passes/CircularInheritanceChecker.cpp Passes/CircularInheritanceChecker.hpp AST/Attr.cpp AST/Attr.hpp AST/Attrs/CustomAttr.cpp AST/Attrs/CustomAttr.hpp AST/Decls/AttributeDecl.cpp AST/Decls/AttributeDecl.hpp AST/Attrs/UnresolvedAttr.cpp AST/Attrs/UnresolvedAttr.hpp AST/Attrs/CopyAttr.cpp AST/Attrs/CopyAttr.hpp AST/Attrs/MoveAttr.cpp AST/Attrs/MoveAttr.hpp AST/Attrs/PodAttr.cpp AST/Attrs/PodAttr.hpp Passes/AttributeResolver.cpp Passes/AttributeResolver.hpp AST/Exprs/ReconstructExpr.cpp AST/Exprs/ReconstructExpr.hpp AST/Exprs/InfixMacroCallExpr.cpp AST/Exprs/InfixMacroCallExpr.hpp AST/Exprs/PrefixMacroCallExpr.cpp AST/Exprs/PrefixMacroCallExpr.hpp AST/Decls/OperatorDecl.cpp AST/Decls/OperatorDecl.hpp AST/Exprs/AssignmentBinaryOperatorExpr.cpp AST/Exprs/AssignmentBinaryOperatorExpr.hpp AST/Exprs/CustomInfixOperatorCallExpr.cpp AST/Exprs/CustomInfixOperatorCallExpr.hpp ASTHelpers/CastHelper.cpp ASTHelpers/CastHelper.hpp AST/Exprs/ConstructTemporaryValueExpr.cpp AST/Exprs/ConstructTemporaryValueExpr.hpp AST/Exprs/CustomPrefixOperatorCallExpr.cpp AST/Exprs/CustomPrefixOperatorCallExpr.hpp AST/Decls/CastOperatorDecl.cpp AST/Decls/CastOperatorDecl.hpp AST/Exprs/CustomCastOperatorCallExpr.cpp AST/Exprs/CustomCastOperatorCallExpr.hpp AST/Decls/IndexOperatorDecl.cpp AST/Decls/IndexOperatorDecl.hpp AST/Decls/CallOperatorDecl.cpp AST/Decls/CallOperatorDecl.hpp AST/Exprs/CustomIndexOperatorCallExpr.cpp AST/Exprs/CustomIndexOperatorCallExpr.hpp AST/Exprs/CustomCallOperatorCallExpr.cpp AST/Exprs/CustomCallOperatorCallExpr.hpp AST/Exprs/CustomPostfixOperatorCallExpr.cpp AST/Exprs/CustomPostfixOperatorCallExpr.hpp)
target_link_libraries(gulc stdc++fs)

target_link_libraries(gulc
        LLVMCore
        LLVMTransformUtils
        LLVMScalarOpts
        LLVMInstCombine
        LLVMObject

        # Ugh there HAS to be an easier way to do this... TODO: Fix this. We don't need more half of these
#        LLVMAArch64AsmParser
#        LLVMAArch64AsmPrinter
#        LLVMAArch64CodeGen
#        LLVMAMDGPUAsmParser
#        LLVMAMDGPUAsmPrinter
#        LLVMAMDGPUCodeGen
#        LLVMARMAsmParser
#        LLVMARMAsmPrinter
#        LLVMARMCodeGen
#        LLVMBPFAsmParser
#        LLVMBPFAsmPrinter
#        LLVMBPFCodeGen
#        LLVMHexagonAsmParser
#        LLVMHexagonCodeGen
#        LLVMLanaiAsmParser
#        LLVMLanaiAsmPrinter
#        LLVMLanaiCodeGen
#        LLVMMipsAsmParser
#        LLVMMipsAsmPrinter
#        LLVMMipsCodeGen
#        LLVMMSP430AsmParser
#        LLVMMSP430AsmPrinter
#        LLVMMSP430CodeGen
#        LLVMNVPTXAsmPrinter
#        LLVMNVPTXCodeGen
#        LLVMPowerPCAsmParser
#        LLVMPowerPCAsmPrinter
#        LLVMPowerPCCodeGen
#        LLVMSparcAsmParser
#        LLVMSparcAsmPrinter
#        LLVMSparcCodeGen
#        LLVMSystemZAsmParser
#        LLVMSystemZAsmPrinter
#        LLVMSystemZCodeGen
#        LLVMWebAssemblyAsmParser
#        LLVMWebAssemblyAsmPrinter
#        LLVMWebAssemblyCodeGen
        LLVMX86AsmParser
        LLVMX86AsmPrinter
        LLVMX86CodeGen
#        LLVMXCoreAsmPrinter
#        LLVMXCoreCodeGen
)

add_compile_definitions(_ALLOW_ITERATOR_DEBUG_LEVEL_MISMATCH)
#add_compile_definitions(_ITERATOR_DEBUG_LEVEL 0)

# Copy some example files into the directory. This just makes it easier for me to test everything is working, this will be removed in the future.
add_custom_command(
        TARGET gulc POST_BUILD
        COMMAND ${CMAKE_COMMAND} -E copy_directory
        ${CMAKE_CURRENT_SOURCE_DIR}/Examples/
        ${CMAKE_CURRENT_BINARY_DIR}/Examples/)
