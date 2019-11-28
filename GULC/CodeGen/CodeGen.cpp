// Copyright (C) 2019 Michael Brandon Huddle
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as published
// by the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public License
// along with this program.  If not, see <https://www.gnu.org/licenses/>.

#include "llvm/ADT/APFloat.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Transforms/InstCombine/InstCombine.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Transforms/Scalar/GVN.h"
#include "llvm/Transforms/Utils.h"

#include <AST/Types/BuiltInType.hpp>
#include <AST/Types/PointerType.hpp>
#include <AST/Exprs/ParenExpr.hpp>
#include <AST/Types/ReferenceType.hpp>
#include <AST/Exprs/CharacterLiteralExpr.hpp>
#include <AST/Types/EnumType.hpp>
#include <AST/Decls/EnumDecl.hpp>
#include <AST/Types/StructType.hpp>
#include <AST/Exprs/RefStructMemberFunctionExpr.hpp>
#include <AST/Types/FlatArrayType.hpp>
#include "CodeGen.hpp"

gulc::Module gulc::CodeGen::generate(gulc::FileAST* file) {
    llvm::LLVMContext* llvmContext = new llvm::LLVMContext();
    llvm::IRBuilder<> irBuilder(*llvmContext);
    // TODO: Should we remove the ending file extension?
    llvm::Module* genModule = new llvm::Module(file->filePath(), *llvmContext);
    //llvm::PassManager<llvm::Function>* funcPassManager = new llvm::PassManager<llvm::Function>();
    llvm::legacy::FunctionPassManager* funcPassManager = new llvm::legacy::FunctionPassManager(genModule);

    // Promote allocas to registers.
    funcPassManager->add(llvm::createPromoteMemoryToRegisterPass());
    // Do simple "peephole" optimizations and bit-twiddling optzns.
    funcPassManager->add(llvm::createInstructionCombiningPass());
    // Reassociate expressions.
    funcPassManager->add(llvm::createReassociatePass());
    // Eliminate Common SubExpressions.
    funcPassManager->add(llvm::createGVNPass());
    // Simplify the control flow graph (deleting unreachable blocks, etc).
    funcPassManager->add(llvm::createCFGSimplificationPass());

    funcPassManager->doInitialization();

    currentFileAst = file;
    this->llvmContext = llvmContext;
    this->irBuilder = &irBuilder;
    this->module = genModule;
    this->funcPass = funcPassManager;

    // Add the externs that have been imported...
    for (const gulc::Decl* decl : file->importExterns()) {
        generateImportExtern(decl);
    }

    for (const gulc::Decl* decl : file->topLevelDecls()) {
        generateDecl(decl);

        //globalObject.print()
    }

    genModule->print(llvm::errs(), nullptr);

    funcPassManager->doFinalization();
    delete funcPassManager;

    return Module(file->filePath(), llvmContext, genModule);
}

void gulc::CodeGen::printError(const std::string &message, TextPosition startPosition, TextPosition endPosition) {
    std::cout << "gulc codegen error[" << currentFileAst->filePath() << ", "
                                    "{" << startPosition.line << ", " << startPosition.column << "} "
                                    "to {" << endPosition.line << ", " << endPosition.column << "}]: "
              << message
              << std::endl;
    std::exit(1);
}

llvm::Type *gulc::CodeGen::generateLlvmType(const gulc::Type* type) {
    switch (type->getTypeKind()) {
        case gulc::Type::Kind::FlatArray: {
            auto flatArrayType = llvm::dyn_cast<gulc::FlatArrayType>(type);
            auto indexType = generateLlvmType(flatArrayType->indexType);
            std::uint64_t length = 0;

            if (!llvm::isa<IntegerLiteralExpr>(flatArrayType->length)) {
                printError("[INTERNAL] `FlatArrayType::indexType` was NOT `IntegerLiteralExpr`, cannot continue!",
                           flatArrayType->startPosition(), flatArrayType->endPosition());
            }

            auto integerSize = llvm::dyn_cast<IntegerLiteralExpr>(flatArrayType->length);

            if (integerSize->numberBase() != 10) {
                printError("[INTERNAL] `IntegerLiteralExpr::numberBase()` was NOT `10` in CodeGen, cannot continue!",
                           integerSize->startPosition(), integerSize->endPosition());
            }

            length = std::stoull(integerSize->numberString);

            return llvm::ArrayType::get(indexType, length);
        }
        case gulc::Type::Kind::BuiltIn: {
            auto builtInType = llvm::dyn_cast<gulc::BuiltInType>(type);

            // Void is special...
            if (builtInType->size() == 0) {
                return llvm::Type::getVoidTy(*llvmContext);
            }

            // Bool is special...
            if (builtInType->isBool()) {
                // TODO: Should we support making booleans int1?
//                return llvm::Type::getInt1Ty(*llvmContext);
                return llvm::Type::getInt8Ty(*llvmContext);
            }

            // Floats...
            if (builtInType->isFloating()) {
                switch (builtInType->size()) {
                    case 2:
                        return llvm::Type::getHalfTy(*llvmContext);
                    case 4:
                        return llvm::Type::getFloatTy(*llvmContext);
                    case 8:
                        return llvm::Type::getDoubleTy(*llvmContext);
                    //case 16: 128 bit is support on some platforms, we should support it...
                    default:
                        printError("unsupported floating point size for type '" + type->getString() + "'!",
                                   type->startPosition(), type->endPosition());
                        return nullptr;
                }
            }

            // Signed/unsigned ints...
            // Thing to note here is that LLVM handles the signedness of a variable by the operation you used, similar to assembly. That is why we don't check if it is signed here.
            switch (builtInType->size()) {
                case 1:
                    return llvm::Type::getInt8Ty(*llvmContext);
                case 2:
                    return llvm::Type::getInt16Ty(*llvmContext);
                case 4:
                    return llvm::Type::getInt32Ty(*llvmContext);
                case 8:
                    return llvm::Type::getInt64Ty(*llvmContext);
                    //case 16: 128 bit is supported on some platforms, we should support it...
                default:
                    printError("unsupported floating point size for type '" + type->getString() + "'!",
                               type->startPosition(), type->endPosition());
                    return nullptr;
            }
        }
        case gulc::Type::Kind::Pointer: {
            auto pointerType = llvm::dyn_cast<gulc::PointerType>(type);
            return llvm::PointerType::getUnqual(generateLlvmType(pointerType->pointToType));
        }
        case gulc::Type::Kind::Reference: {
            auto referenceType = llvm::dyn_cast<gulc::ReferenceType>(type);
            return llvm::PointerType::getUnqual(generateLlvmType(referenceType->referenceToType));
        }
        case gulc::Type::Kind::Enum: {
            auto enumType = llvm::dyn_cast<EnumType>(type);
            return generateLlvmType(enumType->baseType());
        }
        case gulc::Type::Kind::Struct: {
            auto structType = llvm::dyn_cast<StructType>(type);
            return getLlvmStructType(structType->decl());
        }
        default:
            printError("type '" + type->getString() + "' not yet supported!",
                       type->startPosition(), type->endPosition());
            return nullptr;
    }
    return nullptr;
}

std::vector<llvm::Type*> gulc::CodeGen::generateParamTypes(const std::vector<ParameterDecl*>& parameters,
                                                           const StructDecl* parentStruct) {
    std::vector<llvm::Type*> paramTypes{};
    paramTypes.reserve(parameters.size());

    if (parentStruct) {
        paramTypes.push_back(llvm::PointerType::getUnqual(getLlvmStructType(parentStruct)));
    }

    for (const ParameterDecl* parameterDecl : parameters) {
        paramTypes.push_back(generateLlvmType(parameterDecl->type));
    }

    return paramTypes;
}

void gulc::CodeGen::generateImportExtern(const gulc::Decl *decl) {
    switch (decl->getDeclKind()) {
        case gulc::Decl::Kind::Constructor:
            generateExternConstructorDecl(llvm::dyn_cast<ConstructorDecl>(decl));
            break;
        case gulc::Decl::Kind::Destructor:
            generateExternDestructorDecl(llvm::dyn_cast<DestructorDecl>(decl));
            break;
        case gulc::Decl::Kind::Enum:
            // We don't generate any code for the enum declaration...
            break;
        case gulc::Decl::Kind::Function:
            generateExternFunctionDecl(llvm::dyn_cast<FunctionDecl>(decl));
            break;
        case gulc::Decl::Kind::GlobalVariable:
            generateExternGlobalVariableDecl(llvm::dyn_cast<GlobalVariableDecl>(decl));
            break;
        default:
            printError("internal - unsupported imported extern decl!",
                       decl->startPosition(), decl->endPosition());
            break;
    }
}

void gulc::CodeGen::generateDecl(const gulc::Decl *decl, bool isInternal) {
    switch (decl->getDeclKind()) {
        case gulc::Decl::Kind::Enum:
            // We don't generate any code for the enum declaration...
            break;
        case gulc::Decl::Kind::Function:
            generateFunctionDecl(llvm::dyn_cast<gulc::FunctionDecl>(decl), isInternal);
            break;
        case gulc::Decl::Kind::GlobalVariable:
            generateGlobalVariableDecl(llvm::dyn_cast<gulc::GlobalVariableDecl>(decl), isInternal);
            break;
        case gulc::Decl::Kind::Namespace:
            generateNamespace(llvm::dyn_cast<gulc::NamespaceDecl>(decl));
            break;
        case gulc::Decl::Kind::Struct:
            generateStructDecl(llvm::dyn_cast<StructDecl>(decl), isInternal);
            break;
        case gulc::Decl::Kind::TemplateFunction:
            generateTemplateFunctionDecl(llvm::dyn_cast<gulc::TemplateFunctionDecl>(decl), isInternal);
            break;
        default:
            printError("internal - unsupported decl!",
                       decl->startPosition(), decl->endPosition());
            break;
    }
}

void gulc::CodeGen::generateStmt(const gulc::Stmt *stmt, const std::string& stmtName) {
    switch (stmt->getStmtKind()) {
        case gulc::Stmt::Kind::Break:
            return generateBreakStmt(llvm::dyn_cast<BreakStmt>(stmt));
        case gulc::Stmt::Kind::Case:
            printError("`case` not yet supported!", stmt->startPosition(), stmt->endPosition());
            return;
        case gulc::Stmt::Kind::Compound:
            return generateCompoundStmt(llvm::dyn_cast<CompoundStmt>(stmt));
        case gulc::Stmt::Kind::Continue:
            return generateContinueStmt(llvm::dyn_cast<ContinueStmt>(stmt));
        case gulc::Stmt::Kind::Do:
            return generateDoStmt(llvm::dyn_cast<DoStmt>(stmt), stmtName);
        case gulc::Stmt::Kind::For:
            return generateForStmt(llvm::dyn_cast<ForStmt>(stmt), stmtName);
        case gulc::Stmt::Kind::Goto:
            return generateGotoStmt(llvm::dyn_cast<GotoStmt>(stmt));
        case gulc::Stmt::Kind::If:
            return generateIfStmt(llvm::dyn_cast<IfStmt>(stmt));
        case gulc::Stmt::Kind::Labeled:
            return generateLabeledStmt(llvm::dyn_cast<LabeledStmt>(stmt));
        case gulc::Stmt::Kind::Return:
            return generateReturnStmt(llvm::dyn_cast<ReturnStmt>(stmt));
        case gulc::Stmt::Kind::Switch:
            printError("`switch` not yet supported!", stmt->startPosition(), stmt->endPosition());
            return;
        case gulc::Stmt::Kind::Try:
            printError("`try` not yet supported!", stmt->startPosition(), stmt->endPosition());
            return;
        case gulc::Stmt::Kind::TryCatch:
            printError("`catch` not yet supported!", stmt->startPosition(), stmt->endPosition());
            return;
        case gulc::Stmt::Kind::TryFinally:
            printError("`finally` not yet supported!", stmt->startPosition(), stmt->endPosition());
            return;
        case gulc::Stmt::Kind::While:
            return generateWhileStmt(llvm::dyn_cast<WhileStmt>(stmt), stmtName);
        case gulc::Stmt::Kind::Expr:
            generateExpr(llvm::dyn_cast<Expr>(stmt));
            break;
    }
}

llvm::Value* gulc::CodeGen::generateExpr(const Expr* expr) {
    switch (expr->getExprKind()) {
        case gulc::Expr::Kind::BinaryOperator:
            return generateBinaryOperatorExpr(llvm::dyn_cast<BinaryOperatorExpr>(expr));
        case gulc::Expr::Kind::CharacterLiteral:
            printError("character literals not yet supported!",
                       expr->startPosition(), expr->endPosition());
            return nullptr;
        case gulc::Expr::Kind::ExplicitCast:
            printError("explicit casts not yet supported!",
                       expr->startPosition(), expr->endPosition());
            return nullptr;
        case gulc::Expr::Kind::FloatLiteral:
            return generateFloatLiteralExpr(llvm::dyn_cast<FloatLiteralExpr>(expr));
        case gulc::Expr::Kind::FunctionCall:
            return generateFunctionCallExpr(llvm::dyn_cast<FunctionCallExpr>(expr));
        case gulc::Expr::Kind::Identifier:
            return generateIdentifierExpr(llvm::dyn_cast<IdentifierExpr>(expr));
        case gulc::Expr::Kind::ImplicitCast:
            return generateImplicitCastExpr(llvm::dyn_cast<ImplicitCastExpr>(expr));
        case gulc::Expr::Kind::IndexerCall:
            printError("indexer calls not yet supported!",
                       expr->startPosition(), expr->endPosition());
            return nullptr;
        case gulc::Expr::Kind::IntegerLiteral:
            return generateIntegerLiteralExpr(llvm::dyn_cast<IntegerLiteralExpr>(expr));
        case gulc::Expr::Kind::LocalVariableDecl:
            return generateLocalVariableDeclExpr(llvm::dyn_cast<LocalVariableDeclExpr>(expr));
        case gulc::Expr::Kind::MemberAccessCall:
            printError("member access calls not yet supported!",
                       expr->startPosition(), expr->endPosition());
            return nullptr;
        case gulc::Expr::Kind::Paren:
            return generateExpr(llvm::dyn_cast<ParenExpr>(expr)->containedExpr);
        case gulc::Expr::Kind::PostfixOperator:
            return generatePostfixOperatorExpr(llvm::dyn_cast<PostfixOperatorExpr>(expr));
        case gulc::Expr::Kind::PrefixOperator:
            return generatePrefixOperatorExpr(llvm::dyn_cast<PrefixOperatorExpr>(expr));
        case gulc::Expr::Kind::StringLiteral:
            printError("string literals not yet supported!",
                       expr->startPosition(), expr->endPosition());
            return nullptr;
        case gulc::Expr::Kind::Ternary:
            printError("ternary operators not yet supported!",
                       expr->startPosition(), expr->endPosition());
            return nullptr;
        case gulc::Expr::Kind::LValueToRValue:
            return generateLValueToRValue(llvm::dyn_cast<LValueToRValueExpr>(expr));
        case gulc::Expr::Kind::RefLocalVariable:
            return generateRefLocalVariableExpr(llvm::dyn_cast<RefLocalVariableExpr>(expr));
        case gulc::Expr::Kind::RefParameter:
            return generateRefParameterExpr(llvm::dyn_cast<RefParameterExpr>(expr));
        case gulc::Expr::Kind::RefGlobalVariable:
            return generateRefGlobalVariableExpr(llvm::dyn_cast<RefGlobalVariableExpr>(expr));
        case gulc::Expr::Kind::RefEnumConstant:
            return generateRefEnumConstant(llvm::dyn_cast<gulc::RefEnumConstantExpr>(expr));
        case gulc::Expr::Kind::RefStructMemberVariable:
            return generateRefStructMemberVariableExpr(llvm::dyn_cast<gulc::RefStructMemberVariableExpr>(expr));
        case gulc::Expr::Kind::DestructLocalVariable:
            return generateDestructLocalVariableExpr(llvm::dyn_cast<gulc::DestructLocalVariableExpr>(expr));
        case gulc::Expr::Kind::DestructParameter:
            return generateDestructParameterExpr(llvm::dyn_cast<gulc::DestructParameterExpr>(expr));
        case gulc::Expr::Kind::DestructMemberVariable:
            return generateDestructMemberVariableExpr(llvm::dyn_cast<gulc::DestructMemberVariableExpr>(expr));
        case gulc::Expr::Kind::BaseDestructorCall:
            // NOTE: This shouldn't be called by the user. This is only here because of how we handle calling the base
            //       destructor. It is done the same way as the member destructors are called: an Expr list in
            //       `ReturnStmt`
            generateBaseDestructorCallExpr(llvm::dyn_cast<gulc::BaseDestructorCallExpr>(expr));
            return nullptr;
        case gulc::Expr::Kind::RefBase:
            return generateRefBaseExpr(llvm::dyn_cast<RefBaseExpr>(expr));
        default:
            printError("unexpected expression type in code generator!",
                       expr->startPosition(), expr->endPosition());
            return nullptr;
    }
}

// Helpers
void gulc::CodeGen::addBlockAndSetInsertionPoint(llvm::BasicBlock* basicBlock) {
    currentFunction->getBasicBlockList().push_back(basicBlock);
    irBuilder->SetInsertPoint(basicBlock);
}

// Externs
void gulc::CodeGen::generateExternConstructorDecl(const gulc::ConstructorDecl *constructorDecl) {
    std::vector<llvm::Type*> paramTypes = generateParamTypes(constructorDecl->parameters, constructorDecl->parentStruct);
    // Constructors can ONLY return void, they only modify the `this` reference...
    llvm::Type* returnType = llvm::Type::getVoidTy(*llvmContext);
    llvm::FunctionType* functionType = llvm::FunctionType::get(returnType, paramTypes, false);
    llvm::Function* function = llvm::Function::Create(functionType, llvm::Function::LinkageTypes::ExternalLinkage,
                                                      constructorDecl->mangledName(), module);
}

void gulc::CodeGen::generateExternDestructorDecl(const gulc::DestructorDecl *destructorDecl) {
    // Destructors DO NOT support parameters except for the single `this` parameter
    std::vector<llvm::Type*> paramTypes = generateParamTypes({}, destructorDecl->parentStruct);
    // Constructors can ONLY return void, they only modify the `this` reference...
    llvm::Type* returnType = llvm::Type::getVoidTy(*llvmContext);
    llvm::FunctionType* functionType = llvm::FunctionType::get(returnType, paramTypes, false);
    llvm::Function* function = llvm::Function::Create(functionType, llvm::Function::LinkageTypes::ExternalLinkage, destructorDecl->mangledName(), module);
}

void gulc::CodeGen::generateExternFunctionDecl(const FunctionDecl* functionDecl) {
    std::vector<llvm::Type*> paramTypes = generateParamTypes(functionDecl->parameters, functionDecl->parentStruct);
    llvm::Type* returnType = generateLlvmType(functionDecl->resultType);
    llvm::FunctionType* functionType = llvm::FunctionType::get(returnType, paramTypes, false);
    llvm::Function* function = llvm::Function::Create(functionType, llvm::Function::LinkageTypes::ExternalLinkage, functionDecl->mangledName(), module);
}

void gulc::CodeGen::generateExternGlobalVariableDecl(const GlobalVariableDecl* globalVariableDecl) {
    llvm::Type* llvmType = generateLlvmType(globalVariableDecl->type);

    bool isConstant = globalVariableDecl->type->qualifier() == TypeQualifier::Const;

    new llvm::GlobalVariable(*module, llvmType, isConstant, llvm::Function::LinkageTypes::ExternalLinkage,
                             nullptr, globalVariableDecl->mangledName());
}

// Decls
void gulc::CodeGen::generateConstructorDecl(const gulc::ConstructorDecl *constructorDecl, bool isInternal) {
    std::vector<llvm::Type*> paramTypes = generateParamTypes(constructorDecl->parameters, constructorDecl->parentStruct);
    // All constructors return void. We construct the `this` parameter. Memory allocation for the struct is
    // NOT handled by the constructor
    llvm::Type* returnType = llvm::Type::getVoidTy(*llvmContext);
    llvm::FunctionType* functionType = llvm::FunctionType::get(returnType, paramTypes, false);
    llvm::Function* function = module->getFunction(constructorDecl->mangledName());

    if (!function) {
        auto linkageType = llvm::Function::LinkageTypes::ExternalLinkage;

        if (isInternal) {
            linkageType = llvm::Function::LinkageTypes::InternalLinkage;
        }

        function = llvm::Function::Create(functionType, linkageType, constructorDecl->mangledName(), module);
    }

    llvm::BasicBlock* funcBody = llvm::BasicBlock::Create(*llvmContext, "entry", function);
    irBuilder->SetInsertPoint(funcBody);

    setCurrentFunction(function);

    // If there is a base constructor we HAVE to call it as the first line of the constructor
    if (constructorDecl->baseConstructor != nullptr) {
        generateBaseConstructorCallExpr(constructorDecl->baseConstructorCall);
    }

    // Generate the function body
    currentFunctionLocalVariablesCount = 0;
    generateStmt(constructorDecl->body());
    currentFunctionLocalVariablesCount = 0;

    verifyFunction(*function);
    funcPass->run(*function);

    // Reset the insertion point (this probably isn't needed but oh well)
    irBuilder->ClearInsertionPoint();
    currentFunction = nullptr;
}

void gulc::CodeGen::generateDestructorDecl(const gulc::DestructorDecl *destructorDecl, bool isInternal) {
    // Destructors DO NOT support parameters except for the single `this` parameter
    std::vector<llvm::Type*> paramTypes = generateParamTypes({}, destructorDecl->parentStruct);
    // All constructors return void. We construct the `this` parameter. Memory allocation for the struct is
    // NOT handled by the constructor
    llvm::Type* returnType = llvm::Type::getVoidTy(*llvmContext);
    llvm::FunctionType* functionType = llvm::FunctionType::get(returnType, paramTypes, false);
    llvm::Function* function = module->getFunction(destructorDecl->mangledName());

    if (!function) {
        auto linkageType = llvm::Function::LinkageTypes::ExternalLinkage;

        if (isInternal) {
            linkageType = llvm::Function::LinkageTypes::InternalLinkage;
        }

        function = llvm::Function::Create(functionType, linkageType, destructorDecl->mangledName(), module);
    }

    llvm::BasicBlock* funcBody = llvm::BasicBlock::Create(*llvmContext, "entry", function);
    irBuilder->SetInsertPoint(funcBody);

    setCurrentFunction(function);

    // Generate the function body
    currentFunctionLocalVariablesCount = 0;
    generateStmt(destructorDecl->body());
    currentFunctionLocalVariablesCount = 0;

    verifyFunction(*function);
    funcPass->run(*function);

    // Reset the insertion point (this probably isn't needed but oh well)
    irBuilder->ClearInsertionPoint();
    currentFunction = nullptr;
}

void gulc::CodeGen::generateFunctionDecl(const gulc::FunctionDecl *functionDecl, bool isInternal) {
    std::vector<llvm::Type*> paramTypes = generateParamTypes(functionDecl->parameters, functionDecl->parentStruct);
    llvm::Type* returnType = generateLlvmType(functionDecl->resultType);
    llvm::FunctionType* functionType = llvm::FunctionType::get(returnType, paramTypes, false);
    llvm::Function* function = module->getFunction(functionDecl->mangledName());

    if (!function) {
        auto linkageType = llvm::Function::LinkageTypes::ExternalLinkage;

        if (isInternal && !functionDecl->isMain()) {
            linkageType = llvm::Function::LinkageTypes::InternalLinkage;
        }

        function = llvm::Function::Create(functionType, linkageType, functionDecl->mangledName(), module);
    }

    llvm::BasicBlock* funcBody = llvm::BasicBlock::Create(*llvmContext, "entry", function);
    irBuilder->SetInsertPoint(funcBody);
    setCurrentFunction(function);

    // Generate the function body
    currentFunctionLocalVariablesCount = 0;
    generateStmt(functionDecl->body());
    currentFunctionLocalVariablesCount = 0;

    verifyFunction(*function);
    funcPass->run(*function);


    // Reset the insertion point (this probably isn't needed but oh well)
    irBuilder->ClearInsertionPoint();
	currentFunction = nullptr;
}

void gulc::CodeGen::generateGlobalVariableDecl(const gulc::GlobalVariableDecl *globalVariableDecl, bool isInternal) {
    llvm::GlobalVariable* checkExtern = module->getGlobalVariable(globalVariableDecl->name(), true);

    llvm::Constant* initialValue = nullptr;

    if (globalVariableDecl->hasInitialValue()) {
        initialValue = generateConstant(globalVariableDecl->initialValue);
    }

    if (checkExtern) {
        if (initialValue) {
            checkExtern->setInitializer(initialValue);
        }

        return;
    } else {
        llvm::Type* llvmType = generateLlvmType(globalVariableDecl->type);

        bool isConstant = globalVariableDecl->type->qualifier() == TypeQualifier::Const;

        auto linkageType = llvm::Function::LinkageTypes::ExternalLinkage;

        if (isInternal) {
            linkageType = llvm::Function::LinkageTypes::InternalLinkage;
        }

        new llvm::GlobalVariable(*module, llvmType, isConstant, linkageType, initialValue,
                                 globalVariableDecl->mangledName());
    }
}

void gulc::CodeGen::generateNamespace(const gulc::NamespaceDecl *namespaceDecl) {
    const NamespaceDecl* oldNamespace = currentNamespace;
    currentNamespace = namespaceDecl;

    for (const Decl* decl : namespaceDecl->nestedDecls()) {
        generateDecl(decl, false);
    }

    currentNamespace = oldNamespace;
}

void gulc::CodeGen::generateStructDecl(const gulc::StructDecl *structDecl, bool isInternal) {
    const gulc::StructDecl* oldStruct = currentStruct;
    currentStruct = structDecl;

    for (const ConstructorDecl* constructor : structDecl->constructors) {
        // TODO: If the constructor is `private` then `isInternal` must be set to true even if our `isInternal` is false
        //  NOTE: We don't do this for `internal` since `internal` can still be accessed by other objects in the same
        //   project
        generateConstructorDecl(constructor, isInternal);
    }

    for (const Decl* decl : structDecl->members) {
        if (llvm::isa<FunctionDecl>(decl)) {
            generateFunctionDecl(llvm::dyn_cast<FunctionDecl>(decl), isInternal);
        } else if (llvm::isa<TemplateFunctionDecl>(decl)) {
            generateTemplateFunctionDecl(llvm::dyn_cast<TemplateFunctionDecl>(decl), isInternal);
        }
    }

    if (structDecl->destructor != nullptr) {
        generateDestructorDecl(structDecl->destructor, isInternal);
    }

    currentStruct = oldStruct;
}

void gulc::CodeGen::generateTemplateFunctionDecl(const gulc::TemplateFunctionDecl *templateFunctionDecl, bool isInternal) {
    // TODO: We need to support generating the implemented functions in their own file (because the file the `template` is in might already be compiled, making it so we're not compiling the new functions)
    for (FunctionDecl* implementedFunction : templateFunctionDecl->implementedFunctions()) {
        generateDecl(implementedFunction, isInternal);
    }
}

// Stmts
void gulc::CodeGen::generateCompoundStmt(const gulc::CompoundStmt* compoundStmt) {
    unsigned int oldLocalVariableCount = currentFunctionLocalVariablesCount;

    for (const Stmt* stmt : compoundStmt->statements()) {
        generateStmt(stmt);
    }

    currentFunctionLocalVariablesCount = oldLocalVariableCount;
}

void gulc::CodeGen::generateReturnStmt(const gulc::ReturnStmt *returnStmt) {
    if (returnStmt->hasReturnValue()) {
        if (returnStmt->preReturnExprs.empty()) {
            irBuilder->CreateRet(generateExpr(returnStmt->returnValue));
        } else {
            llvm::Value* returnValue = generateExpr(returnStmt->returnValue);

            for (Expr* preReturnExpr : returnStmt->preReturnExprs) {
                generateExpr(preReturnExpr);
            }

            irBuilder->CreateRet(returnValue);
        }
    } else {
        for (Expr* preReturnExpr : returnStmt->preReturnExprs) {
            generateExpr(preReturnExpr);
        }

        irBuilder->CreateRetVoid();
    }
}

void gulc::CodeGen::generateLabeledStmt(const gulc::LabeledStmt *labeledStmt) {
    // Curious why we can't just use normal labels...
    llvm::BasicBlock* labelBody;

    if (currentFunctionLabelsContains(labeledStmt->label())) {
        labelBody = currentFunctionLabels[labeledStmt->label()];
    } else {
        labelBody = llvm::BasicBlock::Create(*llvmContext, labeledStmt->label());
        addCurrentFunctionLabel(labeledStmt->label(), labelBody);
    }

    // We have to explicitly branch to blocks for some reason...
    irBuilder->CreateBr(labelBody);
    addBlockAndSetInsertionPoint(labelBody);

    if (labeledStmt->labeledStmt != nullptr) {
        generateStmt(labeledStmt->labeledStmt, labeledStmt->label());
    }
}

void gulc::CodeGen::generateGotoStmt(const gulc::GotoStmt *gotoStmt) {
    for (Expr* cleanupExpr : gotoStmt->preGotoCleanup) {
        generateExpr(cleanupExpr);
    }

    if (currentFunctionLabelsContains(gotoStmt->label)) {
        irBuilder->CreateBr(currentFunctionLabels[gotoStmt->label]);
    } else {
        llvm::BasicBlock* newBasicBlock = llvm::BasicBlock::Create(*llvmContext, gotoStmt->label);
        irBuilder->CreateBr(newBasicBlock);
        addCurrentFunctionLabel(gotoStmt->label, newBasicBlock);
    }
}

void gulc::CodeGen::generateIfStmt(const gulc::IfStmt *ifStmt) {
    llvm::Value* cond = generateExpr(ifStmt->condition);

    llvm::BasicBlock* trueBlock = llvm::BasicBlock::Create(*llvmContext, "ifTrueBlock", currentFunction);
    llvm::BasicBlock* mergeBlock = llvm::BasicBlock::Create(*llvmContext, "ifMerge");
    llvm::BasicBlock* falseBlock = nullptr;

    // If there isn't a false block we make the IR jump to the merge block on false, else we make an actual false block
    if (ifStmt->hasFalseStmt()) {
        falseBlock = llvm::BasicBlock::Create(*llvmContext, "ifFalseBlock");

        irBuilder->CreateCondBr(cond, trueBlock, falseBlock);
    } else {
        irBuilder->CreateCondBr(cond, trueBlock, mergeBlock);
    }

    // Set the insert point to our true block then generate the statement for it...
    irBuilder->SetInsertPoint(trueBlock);
    if (ifStmt->trueStmt != nullptr) generateStmt(ifStmt->trueStmt);

    // If the last inserted instruction is a branch and we insert a branch after it to the merge block an LLVM pass
    // will mess up and remove the entire if statement...
    if (irBuilder->GetInsertBlock()->getTerminator() == nullptr ||
        !llvm::isa<llvm::BranchInst>(irBuilder->GetInsertBlock()->getTerminator())) {
        irBuilder->CreateBr(mergeBlock);
    }

    // And then add a jump to the merge block if there is a false statement
    if (ifStmt->hasFalseStmt()) {
        // Set the insert point to the false block and then generate the statement for it...
        currentFunction->getBasicBlockList().push_back(falseBlock);
        irBuilder->SetInsertPoint(falseBlock);
        generateStmt(ifStmt->falseStmt);

        // If the last inserted instruction is a branch and we insert a branch after it to the merge block an LLVM pass
        // will mess up and remove the entire if statement...
        if (irBuilder->GetInsertBlock()->getTerminator() == nullptr ||
            !llvm::isa<llvm::BranchInst>(irBuilder->GetInsertBlock()->getTerminator())) {
            // Branch to merge when done...
            irBuilder->CreateBr(mergeBlock);
        }
    }

    // Add the merge block to the function and then set the insert point to it
    currentFunction->getBasicBlockList().push_back(mergeBlock);
    irBuilder->SetInsertPoint(mergeBlock);
}

void gulc::CodeGen::generateWhileStmt(const gulc::WhileStmt *whileStmt, const std::string& loopName) {
    std::string whileName;

    if (loopName.empty()) {
        whileName = "loop" + std::to_string(loopNameNumber);
        ++loopNameNumber;
    } else {
        whileName = loopName;
    }

    llvm::BasicBlock* continueLoop = llvm::BasicBlock::Create(*llvmContext, whileName + "_continue", currentFunction);
    llvm::BasicBlock* loop = llvm::BasicBlock::Create(*llvmContext, whileName + "_loop", currentFunction);
    llvm::BasicBlock* breakLoop = llvm::BasicBlock::Create(*llvmContext, whileName + "_break");

    // For some reason we can't just fall through to the continue loop? We have to explicitly branch to it?
    irBuilder->CreateBr(continueLoop);

    // Set the insert point to the continue loop block and start adding the loop data...
    irBuilder->SetInsertPoint(continueLoop);

    llvm::Value* cond = generateExpr(whileStmt->condition);
    // If the condition is true we continue the loop, if not we break from the loop...
    irBuilder->CreateCondBr(cond, loop, breakLoop);

    // Set the insert point to the loop block for our actual statement...
    irBuilder->SetInsertPoint(loop);

    // We make sure to back up and restore the old loop's break and continue blocks for our `break` and `continue` keywords
    llvm::BasicBlock* oldLoopContinue = currentLoopBlockContinue;
    llvm::BasicBlock* oldLoopBreak = currentLoopBlockBreak;

    currentLoopBlockContinue = continueLoop;
    currentLoopBlockBreak = breakLoop;

    // Generate the loop statement within the loop block then jump back to the continue block...
    enterNestedLoop(continueLoop, breakLoop);
    if (whileStmt->loopStmt != nullptr) generateStmt(whileStmt->loopStmt);
    leaveNestedLoop();
    irBuilder->CreateBr(continueLoop);

    currentLoopBlockContinue = oldLoopContinue;
    currentLoopBlockBreak = oldLoopBreak;

    // Finish by adding the break loop block and setting the insert point to it...
    currentFunction->getBasicBlockList().push_back(breakLoop);
    irBuilder->SetInsertPoint(breakLoop);
}

void gulc::CodeGen::generateDoStmt(const gulc::DoStmt* doStmt, const std::string& loopName) {
    std::string doName;

    if (loopName.empty()) {
        doName = "loop" + std::to_string(loopNameNumber);
        ++loopNameNumber;
    } else {
        doName = loopName;
    }

    llvm::BasicBlock* loop = llvm::BasicBlock::Create(*llvmContext, doName + "_loop", currentFunction);
    llvm::BasicBlock* loopContinue = llvm::BasicBlock::Create(*llvmContext, doName + "_continue");
    llvm::BasicBlock* loopBreak = llvm::BasicBlock::Create(*llvmContext, doName + "_break");

    // For some reason we can't just fall through to the continue loop? We have to explicitly branch to it?
    irBuilder->CreateBr(loop);
    // Set the insert point to the loop block and start adding the loop data...
    irBuilder->SetInsertPoint(loop);

    // We make sure to back up and restore the old loop's break and continue blocks for our `break` and `continue` keywords
    llvm::BasicBlock* oldLoopContinue = currentLoopBlockContinue;
    llvm::BasicBlock* oldLoopBreak = currentLoopBlockBreak;

    currentLoopBlockContinue = loopContinue;
    currentLoopBlockBreak = loopBreak;

    // Generate the statement we loop on...
    enterNestedLoop(loopContinue, loopBreak);
    if (doStmt->loopStmt != nullptr) generateStmt(doStmt->loopStmt);
    leaveNestedLoop();
    irBuilder->CreateBr(loopContinue);

    currentLoopBlockContinue = oldLoopContinue;
    currentLoopBlockBreak = oldLoopBreak;

    // Add the loop continue block to the function and set it as the insert point...
    currentFunction->getBasicBlockList().push_back(loopContinue);
    irBuilder->SetInsertPoint(loopContinue);

    // Generate the condition and create the conditional branch
    llvm::Value* cond = generateExpr(doStmt->condition);
    irBuilder->CreateCondBr(cond, loop, loopBreak);

    // Add the loop break block to the function and set it as the insert point...
    currentFunction->getBasicBlockList().push_back(loopBreak);
    irBuilder->SetInsertPoint(loopBreak);
}

void gulc::CodeGen::generateForStmt(const gulc::ForStmt* forStmt, const std::string& loopName) {
    if (forStmt->preLoop != nullptr) {
        generateExpr(forStmt->preLoop);
    }

    std::string forName;

    if (loopName.empty()) {
        forName = "loop" + std::to_string(loopNameNumber);
        ++loopNameNumber;
    } else {
        forName = loopName;
    }

    llvm::BasicBlock* loop = llvm::BasicBlock::Create(*llvmContext, forName + "_loop", currentFunction);
    llvm::BasicBlock* hiddenContinueLoop = llvm::BasicBlock::Create(*llvmContext, forName + "_hidden_continue");
    llvm::BasicBlock* continueLoop = llvm::BasicBlock::Create(*llvmContext, forName + "_continue");
    llvm::BasicBlock* breakLoop = llvm::BasicBlock::Create(*llvmContext, forName + "_break");

    // Set the loop as the current insert point
    irBuilder->CreateBr(loop);
    irBuilder->SetInsertPoint(loop);

    if (forStmt->condition != nullptr) {
        llvm::Value *cond = generateExpr(forStmt->condition);
        // If the condition is true we continue the loop, if not we break from the loop...
        irBuilder->CreateCondBr(cond, hiddenContinueLoop, breakLoop);
    } else {
        irBuilder->CreateBr(hiddenContinueLoop);
    }

    // Set the hidden continue loop as the current insert point
    currentFunction->getBasicBlockList().push_back(hiddenContinueLoop);
    irBuilder->SetInsertPoint(hiddenContinueLoop);

    // We make sure to back up and restore the old loop's break and continue blocks for our `break` and `continue` keywords
    llvm::BasicBlock* oldLoopContinue = currentLoopBlockContinue;
    llvm::BasicBlock* oldLoopBreak = currentLoopBlockBreak;

    currentLoopBlockContinue = continueLoop;
    currentLoopBlockBreak = breakLoop;

    // Generate the statement we loop on
    enterNestedLoop(continueLoop, breakLoop);
    if (forStmt->loopStmt != nullptr) generateStmt(forStmt->loopStmt);
    leaveNestedLoop();

    currentLoopBlockContinue = oldLoopContinue;
    currentLoopBlockBreak = oldLoopBreak;

    // Now we go to our actual continue block, the continue block has to be here so we apply the 'iterationExpr'
    irBuilder->CreateBr(continueLoop);
    currentFunction->getBasicBlockList().push_back(continueLoop);
    irBuilder->SetInsertPoint(continueLoop);

    // Generate the iteration expression (usually `++i`)
    if (forStmt->iterationExpr != nullptr) generateExpr(forStmt->iterationExpr);

    // Branch back to the beginning of our loop...
    irBuilder->CreateBr(loop);

    // And then finish off by adding the break point.
    currentFunction->getBasicBlockList().push_back(breakLoop);
    irBuilder->SetInsertPoint(breakLoop);

    for (Expr* postLoopCleanupExpr : forStmt->postLoopCleanup) {
        generateExpr(postLoopCleanupExpr);
    }
}

void gulc::CodeGen::generateBreakStmt(const gulc::BreakStmt *breakStmt) {
    if (breakStmt->label().empty()) {
        for (Expr* cleanupExpr : breakStmt->preBreakCleanup) {
            generateExpr(cleanupExpr);
        }

        irBuilder->CreateBr(currentLoopBlockBreak);
    } else {
        llvm::BasicBlock* breakBlock = getBreakBlock(breakStmt->label());

        if (breakBlock == nullptr) {
            printError("[INTERNAL] block '" + breakStmt->label() + "' not found!",
                       breakStmt->startPosition(), breakStmt->endPosition());
            return;
        }

        for (Expr* cleanupExpr : breakStmt->preBreakCleanup) {
            generateExpr(cleanupExpr);
        }

        irBuilder->CreateBr(breakBlock);
    }
}

void gulc::CodeGen::generateContinueStmt(const gulc::ContinueStmt *continueStmt) {
    if (continueStmt->label().empty()) {
        for (Expr* cleanupExpr : continueStmt->preContinueCleanup) {
            generateExpr(cleanupExpr);
        }

        irBuilder->CreateBr(currentLoopBlockContinue);
    } else {
        llvm::BasicBlock* continueBlock = getContinueBlock(continueStmt->label());

        if (continueBlock == nullptr) {
            printError("[INTERNAL] block '" + continueStmt->label() + "' not found!",
                       continueStmt->startPosition(), continueStmt->endPosition());
            return;
        }

        for (Expr* cleanupExpr : continueStmt->preContinueCleanup) {
            generateExpr(cleanupExpr);
        }

        irBuilder->CreateBr(continueBlock);
    }
}

// Exprs
llvm::Constant *gulc::CodeGen::generateConstant(const Expr* expr) {
    if (llvm::isa<gulc::CharacterLiteralExpr>(expr)) {
        return llvm::ConstantInt::get(llvm::Type::getInt8Ty(*llvmContext),
                                      llvm::dyn_cast<CharacterLiteralExpr>(expr)->characterValue());
    } else if (llvm::isa<gulc::FloatLiteralExpr>(expr)) {
        llvm::Type* type = generateLlvmType(expr->resultType);
        return llvm::ConstantFP::get(type, llvm::dyn_cast<FloatLiteralExpr>(expr)->numberValue());
    } else if (llvm::isa<gulc::IntegerLiteralExpr>(expr)) {
        auto intLiteral = llvm::dyn_cast<IntegerLiteralExpr>(expr);
        // TODO: Should we support other types here?...
        return llvm::ConstantInt::get(llvm::Type::getInt32Ty(*llvmContext), intLiteral->numberString, intLiteral->numberBase());
    } else if (llvm::isa<gulc::RefEnumConstantExpr>(expr)) {
        return generateRefEnumConstant(llvm::dyn_cast<gulc::RefEnumConstantExpr>(expr));
    }

    printError("unsupported constant in codegen!", expr->startPosition(), expr->endPosition());
    return nullptr;
}

llvm::Constant *gulc::CodeGen::generateRefEnumConstant(const gulc::RefEnumConstantExpr *expr) {
    if (llvm::isa<EnumType>(expr->resultType)) {
        auto enumType = llvm::dyn_cast<EnumType>(expr->resultType);

        if (enumType->decl()->hasConstants()) {
            for (gulc::EnumConstantDecl *enumConstantDecl : enumType->decl()->enumConstants()) {
                if (enumConstantDecl->name() == expr->constantName()) {
                    return generateConstant(enumConstantDecl->constantValue);
                }
            }
        }

        printError("enum `" + expr->enumName() + "` does not have constant named `" + expr->constantName() + "`!",
                   expr->startPosition(), expr->endPosition());
        return nullptr;
    } else {
        printError("[INTERNAL] enum expression does not have enum return type",
                   expr->startPosition(), expr->endPosition());
        return nullptr;
    }
}

llvm::Value* gulc::CodeGen::generateBinaryOperatorExpr(const gulc::BinaryOperatorExpr *binaryOperatorExpr) {
    llvm::Value* leftValue = generateExpr(binaryOperatorExpr->leftValue);
    llvm::Value* rightValue = generateExpr(binaryOperatorExpr->rightValue);

    gulc::Type* resultType = binaryOperatorExpr->resultType;

    // We handle the binary operators based on the result type...
    if (llvm::isa<EnumType>(resultType)) {
        resultType = llvm::dyn_cast<EnumType>(resultType)->baseType();
    }

    // TODO: Support the `PointerType`
    bool isFloat = false;
    bool isSigned = false;

    if (llvm::isa<BuiltInType>(resultType)) {
        auto builtInType = llvm::dyn_cast<BuiltInType>(resultType);

        isFloat = builtInType->isFloating();
        isSigned = builtInType->isSigned();
    } else if (!(llvm::isa<PointerType>(resultType) ||
                 llvm::isa<ReferenceType>(resultType))) {
        printError("unknown binary operator expression!",
                   binaryOperatorExpr->startPosition(), binaryOperatorExpr->endPosition());
        return nullptr;
    }

    if (binaryOperatorExpr->operatorName() == "=") {
        return irBuilder->CreateStore(rightValue, leftValue, false);
    } else if (binaryOperatorExpr->operatorName() == "+") {
        if (isFloat) {
            return irBuilder->CreateFAdd(leftValue, rightValue, "addtmp");
        } else {
            return irBuilder->CreateAdd(leftValue, rightValue, "addtmp");
        }
    } else if (binaryOperatorExpr->operatorName() == "-") {
        if (isFloat) {
            return irBuilder->CreateFSub(leftValue, rightValue, "subtmp");
        } else {
            return irBuilder->CreateSub(leftValue, rightValue, "subtmp");
        }
    } else if (binaryOperatorExpr->operatorName() == "*") {
        if (isFloat) {
            return irBuilder->CreateFMul(leftValue, rightValue, "multmp");
        } else {
            return irBuilder->CreateMul(leftValue, rightValue, "multmp");
        }
    } else if (binaryOperatorExpr->operatorName() == "/") {
        if (isFloat) {
            return irBuilder->CreateFDiv(leftValue, rightValue, "divtmp");
        } else {
            // TODO: What are the `Exact` variants?
            if (isSigned) {
                return irBuilder->CreateSDiv(leftValue, rightValue, "divtmp");
            } else {
                return irBuilder->CreateUDiv(leftValue, rightValue, "divtmp");
            }
        }
    } else if (binaryOperatorExpr->operatorName() == "%") {
        if (isFloat) {
            return irBuilder->CreateFRem(leftValue, rightValue, "remtmp");
        } else {
            // TODO: What are the `Exact` variants?
            if (isSigned) {
                return irBuilder->CreateSRem(leftValue, rightValue, "remtmp");
            } else {
                return irBuilder->CreateURem(leftValue, rightValue, "remtmp");
            }
        }
    } else if (binaryOperatorExpr->operatorName() == "^") {
        return irBuilder->CreateXor(leftValue, rightValue, "xortmp");
    } else if (binaryOperatorExpr->operatorName() == "<<") {
        return irBuilder->CreateShl(leftValue, rightValue, "shltmp");
    } else if (binaryOperatorExpr->operatorName() == ">>") {
        if (isSigned) {
            return irBuilder->CreateAShr(leftValue, rightValue, "ashrtmp");
        } else {
            return irBuilder->CreateLShr(leftValue, rightValue, "lshrtmp");
        }
    } else if (binaryOperatorExpr->operatorName() == "==") {
        if (isFloat) {
            return irBuilder->CreateFCmpOEQ(leftValue, rightValue, "eqtmp");
        } else {
            return irBuilder->CreateICmpEQ(leftValue, rightValue, "eqtmp");
        }
    } else if (binaryOperatorExpr->operatorName() == ">") {
        if (isFloat) {
            return irBuilder->CreateFCmpOGT(leftValue, rightValue, "gttmp");
        } else {
            if (isSigned) {
                return irBuilder->CreateICmpSGT(leftValue, rightValue, "gttmp");
            } else {
                return irBuilder->CreateICmpUGT(leftValue, rightValue, "gttmp");
            }
        }
    } else if (binaryOperatorExpr->operatorName() == ">=") {
        if (isFloat) {
            return irBuilder->CreateFCmpOGE(leftValue, rightValue, "getmp");
        } else {
            if (isSigned) {
                return irBuilder->CreateICmpSGE(leftValue, rightValue, "getmp");
            } else {
                return irBuilder->CreateICmpUGE(leftValue, rightValue, "getmp");
            }
        }
    } else if (binaryOperatorExpr->operatorName() == "<") {
        if (isFloat) {
            return irBuilder->CreateFCmpOLT(leftValue, rightValue, "lttmp");
        } else {
            if (isSigned) {
                return irBuilder->CreateICmpSLT(leftValue, rightValue, "lttmp");
            } else {
                return irBuilder->CreateICmpULT(leftValue, rightValue, "lttmp");
            }
        }
    } else if (binaryOperatorExpr->operatorName() == "<=") {
        if (isFloat) {
            return irBuilder->CreateFCmpOLE(leftValue, rightValue, "letmp");
        } else {
            if (isSigned) {
                return irBuilder->CreateICmpSLE(leftValue, rightValue, "letmp");
            } else {
                return irBuilder->CreateICmpULE(leftValue, rightValue, "letmp");
            }
        }
    } else {
        printError("binary operator '" + binaryOperatorExpr->operatorName() + "' not yet supported!",
                   binaryOperatorExpr->startPosition(), binaryOperatorExpr->endPosition());
        return nullptr;
    }
}

llvm::Value *gulc::CodeGen::generateIntegerLiteralExpr(const gulc::IntegerLiteralExpr *integerLiteralExpr) {
    if (auto builtInType = llvm::dyn_cast<BuiltInType>(integerLiteralExpr->resultType)) {
        unsigned int numOfBits = builtInType->size() * 8;
        return llvm::ConstantInt::get(*llvmContext, llvm::APInt(numOfBits, integerLiteralExpr->numberString, integerLiteralExpr->numberBase()));
    } else {
        printError("unknown integer literal type!",
                   integerLiteralExpr->startPosition(), integerLiteralExpr->endPosition());
        return nullptr;
    }
}

llvm::Value *gulc::CodeGen::generateFloatLiteralExpr(const gulc::FloatLiteralExpr *floatLiteralExpr) {
    if (auto builtInType = llvm::dyn_cast<BuiltInType>(floatLiteralExpr->resultType)) {
        switch (builtInType->size()) {
            case 2:
                return llvm::ConstantFP::get(*llvmContext, llvm::APFloat(llvm::APFloat::IEEEhalf(), floatLiteralExpr->numberValue()));
            case 4:
                return llvm::ConstantFP::get(*llvmContext, llvm::APFloat(llvm::APFloat::IEEEsingle(), floatLiteralExpr->numberValue()));
            case 8:
                return llvm::ConstantFP::get(*llvmContext, llvm::APFloat(llvm::APFloat::IEEEdouble(), floatLiteralExpr->numberValue()));
            case 16:
                return llvm::ConstantFP::get(*llvmContext, llvm::APFloat(llvm::APFloat::IEEEquad(), floatLiteralExpr->numberValue()));
            default:
                printError("unsupported floating point size!",
                           floatLiteralExpr->startPosition(), floatLiteralExpr->endPosition());
                return nullptr;
        }
    }

    printError("unknown float literal type!",
               floatLiteralExpr->startPosition(), floatLiteralExpr->endPosition());
    return nullptr;
}

llvm::Value *gulc::CodeGen::generateLocalVariableDeclExpr(const gulc::LocalVariableDeclExpr *localVariableDeclExpr) {
    llvm::AllocaInst* result = addLocalVariable(localVariableDeclExpr->name(),
                                                generateLlvmType(localVariableDeclExpr->resultType));

    // If we have a constructor call...
    if (localVariableDeclExpr->foundConstructor) {
        llvm::Function* constructorFunc = module->getFunction(localVariableDeclExpr->foundConstructor->mangledName());

        std::vector<llvm::Value*> llvmArgs{};
        llvmArgs.reserve(localVariableDeclExpr->initializerArgs.size() + 1);
        // We pass a reference to the local variable as the first argument of the constructor
        // the constructor doesn't return anything. It modifies the `this` variable that we pass here
        llvmArgs.push_back(result);

        // Now add the rest of the initializer args (if there are any...)
        for (Expr* initializerArg : localVariableDeclExpr->initializerArgs) {
            llvmArgs.push_back(generateExpr(initializerArg));
        }

        // Call the constructor...
        // We don't bother giving the result a name since constructors return void
        irBuilder->CreateCall(constructorFunc, llvmArgs);

        // Return the local variable reference normally...
    } else if (localVariableDeclExpr->hasInitializer()) {
        // If we didn't find a constructor and there is more than 1 argument then something is wrong...
        if (localVariableDeclExpr->initializerArgs.size() > 1) {
            printError("[INTERNAL] local variable has missing constructor reference!",
                       localVariableDeclExpr->startPosition(), localVariableDeclExpr->endPosition());
        }

        llvm::Value* initialValue = generateExpr(localVariableDeclExpr->initializerArgs[0]);

        // Return the assignment...
        return irBuilder->CreateStore(initialValue, result, false);
    }

    return result;
}

llvm::Value *gulc::CodeGen::generateIdentifierExpr(const gulc::IdentifierExpr *identifierExpr) {
    printError("[INTERNAL] identifier found in codegen, expression not supported!",
               identifierExpr->startPosition(), identifierExpr->endPosition());
    return nullptr;
}

llvm::Value *gulc::CodeGen::generateImplicitCastExpr(const gulc::ImplicitCastExpr *implicitCastExpr) {
    llvm::Value* result = generateExpr(implicitCastExpr->castee);
    castValue(implicitCastExpr->castType, implicitCastExpr->castee->resultType, result);
    return result;
}

llvm::Value *gulc::CodeGen::generateLValueToRValue(const gulc::LValueToRValueExpr *lValueToRValueExpr) {
    llvm::Value* lValue = generateExpr(lValueToRValueExpr->lvalue);

    return irBuilder->CreateLoad(lValue, "l2r");
}

llvm::Value *gulc::CodeGen::generateFunctionCallExpr(const gulc::FunctionCallExpr *functionCallExpr) {
    std::string funcName;
    llvm::Function* func = generateRefFunctionExpr(functionCallExpr->functionReference, &funcName);

    std::vector<llvm::Value*> llvmArgs{};

    if (llvm::isa<RefStructMemberFunctionExpr>(functionCallExpr->functionReference)) {
        auto refStructMemberFunction = llvm::dyn_cast<RefStructMemberFunctionExpr>(functionCallExpr->functionReference);
        llvm::Value* hiddenThis = generateExpr(refStructMemberFunction->objectRef);

        // If the types differ that means there is a compiler cast (one that can never be overridden)
        if (!TypeComparer::getTypesAreSame(refStructMemberFunction->objectRef->resultType,
                                           refStructMemberFunction->structType, true)) {
            hiddenThis = irBuilder->CreateBitCast(hiddenThis,
                                                 llvm::PointerType::getUnqual(generateLlvmType(refStructMemberFunction->structType)));
        }

        llvmArgs.push_back(hiddenThis);
    }

    if (functionCallExpr->hasArguments()) {
        llvmArgs.reserve(functionCallExpr->arguments.size());

        for (gulc::Expr *arg : functionCallExpr->arguments) {
            llvmArgs.push_back(generateExpr(arg));
        }
    }

    llvm::Value* result = irBuilder->CreateCall(func, llvmArgs, funcName + "_result");

//    if (!func->getReturnType()->isVoidTy()) {
//        llvm::AllocaInst *retValue = irBuilder->CreateAlloca(func->getReturnType(), nullptr, funcName + "_result");
//        irBuilder->CreateStore(result, retValue);
//        result = retValue;
//    }


    return result;
}

llvm::Value *gulc::CodeGen::generatePrefixOperatorExpr(const gulc::PrefixOperatorExpr *prefixOperatorExpr) {
    gulc::Type* exprResultType = prefixOperatorExpr->expr->resultType;

    if (llvm::isa<gulc::BuiltInType>(exprResultType)) {
        auto builtInType = llvm::dyn_cast<gulc::BuiltInType>(exprResultType);

        llvm::Value* lvalue = generateExpr(prefixOperatorExpr->expr);
        llvm::Value* rvalue = irBuilder->CreateLoad(lvalue, "l2r");

        if (prefixOperatorExpr->operatorName() == "++") {
            if (builtInType->isFloating()) {
                llvm::Value* newValue = irBuilder->CreateFAdd(rvalue, llvm::ConstantFP::get(*llvmContext, llvm::APFloat(1.0f)), "preinctmp");
                irBuilder->CreateStore(newValue, lvalue);
            } else {
                llvm::Value* newValue = irBuilder->CreateAdd(rvalue, llvm::ConstantInt::get(*llvmContext, llvm::APInt(builtInType->size() * 8, 1)), "preinctmp");
                irBuilder->CreateStore(newValue, lvalue);
            }
        } else if (prefixOperatorExpr->operatorName() == "--") {
            if (builtInType->isFloating()) {
                llvm::Value* newValue = irBuilder->CreateFSub(rvalue, llvm::ConstantFP::get(*llvmContext, llvm::APFloat(1.0f)), "predectmp");
                irBuilder->CreateStore(newValue, lvalue);
            } else {
                llvm::Value* newValue = irBuilder->CreateSub(rvalue, llvm::ConstantInt::get(*llvmContext, llvm::APInt(builtInType->size() * 8, 1)), "predectmp");
                irBuilder->CreateStore(newValue, lvalue);
            }
        } else if (prefixOperatorExpr->operatorName() == "-") {
            if (builtInType->isFloating()) {
                return irBuilder->CreateFNeg(rvalue, "negtmp");
            } else {
                return irBuilder->CreateNeg(rvalue, "negtmp");
            }
        } else if (prefixOperatorExpr->operatorName() == "&" || prefixOperatorExpr->operatorName() == ".ref") {
            // NOTE: All error checking for this should be performed in a pass before the code generator
            return lvalue;
        } else if (prefixOperatorExpr->operatorName() != "+") {
            printError("unknown built in prefix operator '" + prefixOperatorExpr->operatorName() +  "'!",
                       prefixOperatorExpr->startPosition(), prefixOperatorExpr->endPosition());
        }

        return lvalue;
    } else if (llvm::isa<PointerType>(exprResultType)) {
        llvm::Value *lvalue = generateExpr(prefixOperatorExpr->expr);

        if (prefixOperatorExpr->operatorName() == "*") {
            return irBuilder->CreateLoad(lvalue, "deref");
        } else if (prefixOperatorExpr->operatorName() == "++" || prefixOperatorExpr->operatorName() == "--") {
            // TODO: We need to know the size of a pointer to support this...
            printError("increment and decrement operators not yet supported on pointer types!",
                       prefixOperatorExpr->startPosition(), prefixOperatorExpr->endPosition());
        } else {
            printError("unknown prefix operator '" + prefixOperatorExpr->operatorName() + "' used on pointer type!",
                       prefixOperatorExpr->startPosition(), prefixOperatorExpr->endPosition());
        }
    } else if (llvm::isa<ReferenceType>(exprResultType)) {
        llvm::Value *lvalue = generateExpr(prefixOperatorExpr->expr);

        if (prefixOperatorExpr->operatorName() == ".deref") {
            return irBuilder->CreateLoad(lvalue, "deref");
            //return lvalue;
        } else if (prefixOperatorExpr->operatorName() == "++" || prefixOperatorExpr->operatorName() == "--") {
            // TODO: We need to know the size of a pointer to support this...
            printError("increment and decrement operators not yet supported on reference types!",
                       prefixOperatorExpr->startPosition(), prefixOperatorExpr->endPosition());
        } else {
            printError("unknown prefix operator '" + prefixOperatorExpr->operatorName() + "' used on reference type!",
                       prefixOperatorExpr->startPosition(), prefixOperatorExpr->endPosition());
        }
    } else {
        printError("built in prefix operator called on unsupported type!",
                   prefixOperatorExpr->startPosition(), prefixOperatorExpr->endPosition());
    }

    return nullptr;
}

llvm::Value *gulc::CodeGen::generatePostfixOperatorExpr(const gulc::PostfixOperatorExpr *postfixOperatorExpr) {
    if (!llvm::isa<gulc::BuiltInType>(postfixOperatorExpr->resultType)) {
        printError("built in postfix operator called on non-built in type!",
                   postfixOperatorExpr->startPosition(), postfixOperatorExpr->endPosition());
        return nullptr;
    }

    auto builtInType = llvm::dyn_cast<gulc::BuiltInType>(postfixOperatorExpr->resultType);

    llvm::Value* lvalue = generateExpr(postfixOperatorExpr->expr);
    llvm::Value* rvalue = irBuilder->CreateLoad(lvalue, "l2r");

    if (postfixOperatorExpr->operatorName() == "++") {
        if (builtInType->isFloating()) {
            llvm::Value* newValue = irBuilder->CreateFAdd(rvalue, llvm::ConstantFP::get(*llvmContext, llvm::APFloat(1.0f)), "preinctmp");
            irBuilder->CreateStore(newValue, lvalue);
        } else {
            llvm::Value* newValue = irBuilder->CreateAdd(rvalue, llvm::ConstantInt::get(*llvmContext, llvm::APInt(builtInType->size() * 8, 1)), "preinctmp");
            irBuilder->CreateStore(newValue, lvalue);
        }
    } else if (postfixOperatorExpr->operatorName() == "--") {
        if (builtInType->isFloating()) {
            llvm::Value* newValue = irBuilder->CreateFSub(rvalue, llvm::ConstantFP::get(*llvmContext, llvm::APFloat(1.0f)), "predectmp");
            irBuilder->CreateStore(newValue, lvalue);
        } else {
            llvm::Value* newValue = irBuilder->CreateSub(rvalue, llvm::ConstantInt::get(*llvmContext, llvm::APInt(builtInType->size() * 8, 1)), "predectmp");
            irBuilder->CreateStore(newValue, lvalue);
        }
    } else {
        printError("unknown built in postfix operator!",
                   postfixOperatorExpr->startPosition(), postfixOperatorExpr->endPosition());
    }

    return rvalue;
}

llvm::Value *gulc::CodeGen::generateRefLocalVariableExpr(const gulc::RefLocalVariableExpr *refLocalVariableExpr) {
    llvm::AllocaInst* localVariableAlloca = getLocalVariableOrNull(refLocalVariableExpr->name());

    if (localVariableAlloca != nullptr) {
        return localVariableAlloca;
    } else {
        printError("[INTERNAL] local variable was not found!",
                   refLocalVariableExpr->startPosition(), refLocalVariableExpr->endPosition());
    }

    return nullptr;
}

llvm::Value *gulc::CodeGen::generateRefParameterExpr(const gulc::RefParameterExpr *refParameterExpr) {
    if (refParameterExpr->paramIndex() < currentFunctionParameters.size()) {
        return currentFunctionParameters[refParameterExpr->paramIndex()];
    }

    printError("[INTERNAL] parameter was not found!",
               refParameterExpr->startPosition(), refParameterExpr->endPosition());
    return nullptr;
}

llvm::Value *gulc::CodeGen::generateRefGlobalVariableExpr(const gulc::RefGlobalVariableExpr *refGlobalFileVariableExpr) {
    // TODO: Should `AllowInternal` be true?
    return module->getGlobalVariable(refGlobalFileVariableExpr->globalVariable()->mangledName(), true);
}

llvm::Value *gulc::CodeGen::generateRefStructMemberVariableExpr(const gulc::RefStructMemberVariableExpr *refStructMemberVariableExpr) {
    llvm::Value* objectRef = generateExpr(refStructMemberVariableExpr->objectRef);

    // If the types differ that means there is a compiler cast (one that can never be overridden)
    if (!TypeComparer::getTypesAreSame(refStructMemberVariableExpr->objectRef->resultType,
                                       refStructMemberVariableExpr->structType, true)) {
        objectRef = irBuilder->CreateBitCast(objectRef,
                llvm::PointerType::getUnqual(generateLlvmType(refStructMemberVariableExpr->structType)));
    }

    std::vector<GlobalVariableDecl*>& dataMembers = refStructMemberVariableExpr->structType->decl()->dataMembers;
    unsigned int index = 0;
    bool elementFound = false;

    for (std::size_t i = 0; i < dataMembers.size(); ++i) {
        // We check if the pointers are the same for equality...
        if (dataMembers[i] == refStructMemberVariableExpr->refVariable) {
            index = i;
            elementFound = true;
            break;
        }
    }

    if (!elementFound) {
        printError("struct element '" + refStructMemberVariableExpr->refVariable->name() + "' was not found!",
                   refStructMemberVariableExpr->startPosition(), refStructMemberVariableExpr->endPosition());
    }

    // If the struct has a base type we increment it by one to account for the base class member
    if (refStructMemberVariableExpr->structType->decl()->baseStruct != nullptr) {
        index += 1;
    }

    // NOTE: Not exactly sure whats wrong here but we'll just let LLVM handle getting the type...
//    llvm::StructType* structType = getLlvmStructType(refStructMemberVariableExpr->structType->decl());
//    llvm::PointerType* structPointerType = llvm::PointerType::getUnqual(structType);
    return irBuilder->CreateStructGEP(nullptr, objectRef, index);
}

llvm::Function *gulc::CodeGen::generateRefFunctionExpr(const gulc::Expr *expr, std::string *nameOut) {
    switch (expr->getExprKind()) {
        case gulc::Expr::Kind::RefFunction: {
            auto refFileFunction = llvm::dyn_cast<RefFunctionExpr>(expr);
            *nameOut = refFileFunction->function()->name();
            return module->getFunction(refFileFunction->function()->mangledName());
        }
        case gulc::Expr::Kind::RefStructMemberFunction: {
            auto refStructMemberFunction = llvm::dyn_cast<RefStructMemberFunctionExpr>(expr);
            *nameOut = refStructMemberFunction->refFunction->name();
            return module->getFunction(refStructMemberFunction->refFunction->mangledName());
        }
        default:
            printError("[INTERNAL] unsupported function reference!",
                       expr->startPosition(), expr->endPosition());
            return nullptr;
    }
}

llvm::Value *gulc::CodeGen::generateDestructLocalVariableExpr(const gulc::DestructLocalVariableExpr *destructLocalVariableExpr) {
    llvm::AllocaInst* variableRef = getLocalVariableOrNull(destructLocalVariableExpr->localVariable->name());

    llvm::Function* destructorFunc = module->getFunction(destructLocalVariableExpr->destructor->mangledName());

    std::vector<llvm::Value*> llvmArgs{};
    llvmArgs.reserve(1);
    // We pass a reference to the local variable as the first argument of the destructor
    // the destructor doesn't return anything. It modifies the `this` variable that we pass here
    llvmArgs.push_back(variableRef);

    // Call the destructor...
    // We don't bother giving the result a name since destructors return void
    irBuilder->CreateCall(destructorFunc, llvmArgs);

    // We technically just return null...
    return variableRef;
}

llvm::Value *gulc::CodeGen::generateDestructParameterExpr(const gulc::DestructParameterExpr *destructParameterExpr) {
    llvm::Value* parameterRef = generateRefParameterExpr(destructParameterExpr->parameter);

    llvm::Function* destructorFunc = module->getFunction(destructParameterExpr->destructor->mangledName());

    std::vector<llvm::Value*> llvmArgs{};
    llvmArgs.reserve(1);
    // We pass a reference to the local variable as the first argument of the destructor
    // the destructor doesn't return anything. It modifies the `this` variable that we pass here
    llvmArgs.push_back(parameterRef);

    // Call the destructor...
    // We don't bother giving the result a name since destructors return void
    irBuilder->CreateCall(destructorFunc, llvmArgs);

    // We technically just return null...
    return parameterRef;
}

llvm::Value *gulc::CodeGen::generateDestructMemberVariableExpr(const gulc::DestructMemberVariableExpr *destructMemberVariableExpr) {
    llvm::Value* memberVariableRef = generateRefStructMemberVariableExpr(destructMemberVariableExpr->memberVariable);

    llvm::Function* destructorFunc = module->getFunction(destructMemberVariableExpr->destructor->mangledName());

    std::vector<llvm::Value*> llvmArgs{};
    llvmArgs.reserve(1);
    // We pass a reference to the local variable as the first argument of the destructor
    // the destructor doesn't return anything. It modifies the `this` variable that we pass here
    llvmArgs.push_back(memberVariableRef);

    // Call the destructor...
    // We don't bother giving the result a name since destructors return void
    irBuilder->CreateCall(destructorFunc, llvmArgs);

    // We technically just return null...
    return memberVariableRef;
}

llvm::Value *gulc::CodeGen::generateRefBaseExpr(const gulc::RefBaseExpr *refBaseExpr) {
    llvm::Value* refThis = generateExpr(refBaseExpr->refThis);
    // We implicitly convert to a pointer here because technically in the IR `refThis` is a pointer even though
    // in the AST it is not a pointer. There might be a bug here.
    return irBuilder->CreateBitCast(refThis,
                                    llvm::PointerType::getUnqual(generateLlvmType(refBaseExpr->resultType)));
}

void gulc::CodeGen::generateBaseConstructorCallExpr(const gulc::BaseConstructorCallExpr *baseConstructorCallExpr) {
    llvm::Function* baseConstructorFunc = module->getFunction(baseConstructorCallExpr->baseConstructor->mangledName());

    std::vector<llvm::Value*> llvmArgs{};
    llvmArgs.reserve(baseConstructorCallExpr->arguments.size() + 1);

    gulc::RefParameterExpr refThisExpr({}, {}, 0);
    llvm::Value* refThis = generateRefParameterExpr(&refThisExpr);
    llvm::Value* derefThis = irBuilder->CreateLoad(refThis, "deref");

    // The `0` index is our base struct. This is the same as bitcasting to the base type (and an llvm pass will even
    // convert the bitcast to the member access)
    llvm::Value* thisCastedToBase = irBuilder->CreateStructGEP(nullptr, derefThis, 0);

    llvmArgs.push_back(thisCastedToBase);

    for (Expr* argument : baseConstructorCallExpr->arguments) {
        llvmArgs.push_back(generateExpr(argument));
    }

    irBuilder->CreateCall(baseConstructorFunc, llvmArgs);
}

void gulc::CodeGen::generateBaseDestructorCallExpr(const gulc::BaseDestructorCallExpr *baseDestructorCallExpr) {
    llvm::Function* baseDestructorFunc = module->getFunction(baseDestructorCallExpr->baseDestructor->mangledName());

    std::vector<llvm::Value*> llvmArgs{};
    llvmArgs.reserve(1);

    gulc::RefParameterExpr refThisExpr({}, {}, 0);
    llvm::Value* refThis = generateRefParameterExpr(&refThisExpr);
    llvm::Value* derefThis = irBuilder->CreateLoad(refThis, "deref");

    // The `0` index is our base struct. This is the same as bitcasting to the base type (and an llvm pass will even
    // convert the bitcast to the member access)
    llvm::Value* thisCastedToBase = irBuilder->CreateStructGEP(nullptr, derefThis, 0);

    llvmArgs.push_back(thisCastedToBase);

    irBuilder->CreateCall(baseDestructorFunc, llvmArgs);
}

void gulc::CodeGen::castValue(gulc::Type *to, gulc::Type *from, llvm::Value*& value) {
    if (llvm::isa<BuiltInType>(from)) {
        auto fromBuiltIn = llvm::dyn_cast<BuiltInType>(from);

        if (llvm::isa<BuiltInType>(to)) {
            auto toBuiltIn = llvm::dyn_cast<BuiltInType>(to);

            if (fromBuiltIn->isFloating()) {
                if (toBuiltIn->isFloating()) {
                    if (toBuiltIn->size() > fromBuiltIn->size()) {
                        value = irBuilder->CreateFPExt(value, generateLlvmType(to), "fpext");
                        return;
                    } else {
                        value = irBuilder->CreateFPTrunc(value, generateLlvmType(to), "fptrunc");
                        return;
                    }
                } else if (toBuiltIn->isSigned()) {
                    value = irBuilder->CreateFPToSI(value, generateLlvmType(to), "fp2si");
                    return;
                } else {
                    value = irBuilder->CreateFPToUI(value, generateLlvmType(to), "fp2ui");
                    return;
                }
            } else if (fromBuiltIn->isSigned()) {
                if (toBuiltIn->isFloating()) {
                    value = irBuilder->CreateSIToFP(value, generateLlvmType(to), "si2fp");
                    return;
                } else {
                    // TODO: I'm not sure if the `isSigned` is meant for `value` or `destTy`? Assuming value...
                    value = irBuilder->CreateIntCast(value, generateLlvmType(to), fromBuiltIn->isSigned(), "si2int");
                    return;
                }
            } else {
                if (toBuiltIn->isFloating()) {
                    value = irBuilder->CreateUIToFP(value, generateLlvmType(to), "ui2fp");
                    return;
                } else {
                    // TODO: I'm not sure if the `isSigned` is meant for `value` or `destTy`? Assuming value...
                    value = irBuilder->CreateIntCast(value, generateLlvmType(to), fromBuiltIn->isSigned(), "ui2int");
                    return;
                }
            }
        } else if (llvm::isa<PointerType>(to)) {
            if (fromBuiltIn->isFloating()) {
                printError("[INTERNAL] casting from a float to a pointer is NOT supported!",
                           to->startPosition(), to->endPosition());
                return;
            }

            value = irBuilder->CreateIntToPtr(value, generateLlvmType(to), "int2ptr");
            return;
        }
    } else if (llvm::isa<gulc::ReferenceType>(from)) {
        // We assume if it reaches this point that the cast is valid
        if (llvm::isa<gulc::ReferenceType>(to)) {
            value = irBuilder->CreateBitCast(value, generateLlvmType(to));
            return;
        } else {
            printError("[INTERNAL] reference '" + from->getString() + "' to non-reference '" + to->getString() + "' cast found in code gen!",
                       to->startPosition(), to->endPosition());
        }
    }

    // TODO: We need to have the start and end positions passed to us
    printError("casting to type `" + to->getString() + "` from type `" + from->getString() + "` is not supported!",
               to->startPosition(), to->endPosition());
}
