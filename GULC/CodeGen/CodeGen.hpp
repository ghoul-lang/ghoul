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

#ifndef GULC_CODEGEN_HPP
#define GULC_CODEGEN_HPP

#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LegacyPassManager.h>

#include <AST/FileAST.hpp>
#include <AST/Decls/FunctionDecl.hpp>
#include <AST/Exprs/BinaryOperatorExpr.hpp>
#include <AST/Exprs/IntegerLiteralExpr.hpp>
#include <AST/Exprs/FloatLiteralExpr.hpp>
#include <AST/Stmts/ReturnStmt.hpp>
#include <AST/Stmts/LabeledStmt.hpp>
#include <AST/Stmts/GotoStmt.hpp>
#include <AST/Exprs/LocalVariableDeclExpr.hpp>
#include <AST/Exprs/IdentifierExpr.hpp>
#include <AST/Exprs/LValueToRValueExpr.hpp>
#include <AST/Exprs/FunctionCallExpr.hpp>
#include <AST/Stmts/IfStmt.hpp>
#include <AST/Stmts/WhileStmt.hpp>
#include <AST/Stmts/DoStmt.hpp>
#include <AST/Stmts/ForStmt.hpp>
#include <AST/Exprs/PrefixOperatorExpr.hpp>
#include <AST/Exprs/PostfixOperatorExpr.hpp>
#include <AST/Stmts/BreakStmt.hpp>
#include <AST/Stmts/ContinueStmt.hpp>
#include <AST/Exprs/ImplicitCastExpr.hpp>
#include <AST/Exprs/RefFunctionExpr.hpp>
#include <AST/Exprs/RefLocalVariableExpr.hpp>
#include <AST/Exprs/RefParameterExpr.hpp>
#include <AST/Decls/GlobalVariableDecl.hpp>
#include <AST/Exprs/RefGlobalVariableExpr.hpp>
#include <AST/Decls/EnumConstantDecl.hpp>
#include <AST/Exprs/RefEnumConstantExpr.hpp>
#include <AST/Decls/NamespaceDecl.hpp>
#include <AST/Decls/TemplateFunctionDecl.hpp>
#include <AST/Decls/StructDecl.hpp>
#include <AST/Exprs/RefStructMemberVariableExpr.hpp>
#include <AST/Exprs/DestructLocalVariableExpr.hpp>
#include <AST/Exprs/DestructParameterExpr.hpp>
#include <AST/Exprs/DestructMemberVariableExpr.hpp>
#include <AST/Exprs/BaseDestructorCallExpr.hpp>
#include <AST/Exprs/RefBaseExpr.hpp>
#include <Targets/Target.hpp>
#include <ASTHelpers/SizeofHelper.hpp>
#include <AST/Stmts/ConstructStructMemberVariableStmt.hpp>
#include <AST/Exprs/ReconstructExpr.hpp>
#include <AST/Exprs/AssignmentBinaryOperatorExpr.hpp>
#include <AST/Exprs/CustomInfixOperatorCallExpr.hpp>
#include <AST/Exprs/ConstructTemporaryValueExpr.hpp>
#include <AST/Exprs/CustomPrefixOperatorCallExpr.hpp>
#include <AST/Exprs/CustomCastOperatorCallExpr.hpp>
#include <AST/Exprs/CustomIndexOperatorCallExpr.hpp>
#include <AST/Exprs/CustomCallOperatorCallExpr.hpp>
#include <AST/Exprs/CustomPostfixOperatorCallExpr.hpp>
#include "Module.hpp"

namespace gulc {
    // This is a storage container for any temporary value that might require destruction at the end of a statement
    // E.g. this will hold function results, temporary constructor results, etc.
    struct TemporaryValue {
        gulc::Type* gulType;
        llvm::AllocaInst* llvmReference;

        TemporaryValue(gulc::Type* gulType, llvm::AllocaInst* llvmReference)
                : gulType(gulType), llvmReference(llvmReference) {}

    };

    class CodeGen {
    private:
        Target* genTarget;

    public:
        explicit CodeGen(Target* genTarget)
                : genTarget(genTarget), currentFileAst(nullptr), currentStruct(nullptr), currentNamespace(nullptr),
                  llvmContext(nullptr), irBuilder(nullptr), module(nullptr), funcPass(nullptr),
                  loopNameNumber(0),
                  currentFunction(nullptr), currentFunctionParameters(), entryBlockBuilder(nullptr),
                  currentFunctionLabels(), currentFunctionLocalVariablesCount(0),  currentFunctionLocalVariables(),
                  currentLoopBlockContinue(nullptr), currentLoopBlockBreak(nullptr),
                  nestedLoopCount(0), nestedLoopContinues(), nestedLoopBreaks(), llvmStructTypes(),
                  temporaryValues() {}

        gulc::Module generate(FileAST* file);

    private:
        void printError(const std::string& message, TextPosition startPosition, TextPosition endPosition);

        void generateImportExtern(const Decl* decl);
        void generateDecl(const Decl* decl, bool isInternal = true);
        void generateStmt(const Stmt* stmt, const std::string& stmtName = "");
        llvm::Value* generateExpr(const Expr* expr);

        // Helpers
        void addBlockAndSetInsertionPoint(llvm::BasicBlock* basicBlock);

        // Externs
        void generateExternConstructorDecl(const ConstructorDecl* constructorDecl);
        void generateExternDestructorDecl(const DestructorDecl* destructorDecl);
        void generateExternFunctionDecl(const FunctionDecl* functionDecl);
        void generateExternGlobalVariableDecl(const GlobalVariableDecl* globalVariableDecl);

        // Decls
        void generateConstructorDecl(const ConstructorDecl* constructorDecl, bool isInternal);
        void generateDestructorDecl(const DestructorDecl* destructorDecl, bool isInternal);
        void generateFunctionDecl(const FunctionDecl* functionDecl, bool isInternal);
        void generateGlobalVariableDecl(const GlobalVariableDecl* globalVariableDecl, bool isInternal);
        void generateNamespace(const NamespaceDecl* namespaceDecl);
        void generateStructDecl(const StructDecl* structDecl, bool isInternal);
        void generateTemplateFunctionDecl(const TemplateFunctionDecl* templateFunctionDecl, bool isInternal);

        // Stmts
        void generateCompoundStmt(const CompoundStmt* compoundStmt);
        void generateReturnStmt(const ReturnStmt* returnStmt);
        void generateLabeledStmt(const LabeledStmt* labeledStmt);
        void generateGotoStmt(const GotoStmt* gotoStmt);
        void generateIfStmt(const IfStmt* ifStmt);
        void generateWhileStmt(const WhileStmt* whileStmt, const std::string& loopName);
        void generateDoStmt(const DoStmt* doStmt, const std::string& loopName);
        void generateForStmt(const ForStmt* forStmt, const std::string& loopName);
        void generateBreakStmt(const BreakStmt* breakStmt);
        void generateContinueStmt(const ContinueStmt* continueStmt);
        void generateConstructStructMemberVariableStmt(const ConstructStructMemberVariableStmt* constructStructMemberVariableStmt);

        // Types
        llvm::Type* generateLlvmType(const gulc::Type* type);
        std::vector<llvm::Type*> generateParamTypes(const std::vector<ParameterDecl*>& parameters,
                                                    const StructDecl* parentStruct);

        // Exprs
        llvm::Constant* generateConstant(const Expr* expr);
        llvm::Constant* generateRefEnumConstant(const RefEnumConstantExpr* expr);

        llvm::Value* generateAssignmentBinaryOperatorExpr(const AssignmentBinaryOperatorExpr* assignmentBinaryOperatorExpr);
        llvm::Value* generateBuiltInBinaryOperation(gulc::Type* type, std::string const& operatorName,
                                                    llvm::Value* leftValue, llvm::Value* rightValue,
                                                    TextPosition const& startPosition, TextPosition const& endPosition);
        llvm::Value* generateBinaryOperatorExpr(const BinaryOperatorExpr* binaryOperatorExpr);
        llvm::Value* generateIntegerLiteralExpr(const IntegerLiteralExpr* integerLiteralExpr);
        llvm::Value* generateFloatLiteralExpr(const FloatLiteralExpr* floatLiteralExpr);
        llvm::Value* generateLocalVariableDeclExpr(const LocalVariableDeclExpr* localVariableDeclExpr);
        llvm::Value* generateIdentifierExpr(const IdentifierExpr* identifierExpr);
        llvm::Value* generateImplicitCastExpr(const ImplicitCastExpr* implicitCastExpr);
        llvm::Value* generateLValueToRValue(const LValueToRValueExpr* lValueToRValueExpr);
        llvm::Value* generateFunctionCallExpr(const FunctionCallExpr* functionCallExpr);
        llvm::Value* generatePrefixOperatorExpr(const PrefixOperatorExpr* prefixOperatorExpr);
        llvm::Value* generatePostfixOperatorExpr(const PostfixOperatorExpr* postfixOperatorExpr);
        llvm::Value* generateRefLocalVariableExpr(const RefLocalVariableExpr* refLocalVariableExpr);
        llvm::Value* generateRefParameterExpr(const RefParameterExpr* refParameterExpr);
        llvm::Value* generateRefGlobalVariableExpr(const RefGlobalVariableExpr* refGlobalFileVariableExpr);
        llvm::Value* generateRefStructMemberVariableExpr(const RefStructMemberVariableExpr* refStructMemberVariableExpr);
        llvm::Value* getVTableFunctionPointer(gulc::StructDecl* structDecl, llvm::Value* objectRef,
                                              std::size_t vtableIndex, llvm::FunctionType* functionType);
        llvm::Value* generateRefFunctionExpr(const Expr* expr);
        void destructStruct(llvm::Value* structRef, DestructorDecl* destructor);
        llvm::Value* generateDestructLocalVariableExpr(const DestructLocalVariableExpr* destructLocalVariableExpr);
        llvm::Value* generateDestructParameterExpr(const DestructParameterExpr* destructParameterExpr);
        llvm::Value* generateDestructMemberVariableExpr(const DestructMemberVariableExpr* destructMemberVariableExpr);
        llvm::Value* generateRefBaseExpr(const RefBaseExpr* refBaseExpr);
        llvm::Value* generateReconstructExpr(const ReconstructExpr* reconstructExpr);
        // Gets either the vtable reference or the operator function pointer, NOTE: This accepts a function decl
        // as we use this for both `CastOperatorDecl` and `OperatorDecl`
        llvm::Value* generateRefOperatorExpr(bool isVTableCall, FunctionDecl* functionDecl,
                                             llvm::Value* objectRef,
                                             TextPosition const& startPosition,
                                             TextPosition const& endPosition);
        llvm::Value* generateCustomInfixOperatorCallExpr(const CustomInfixOperatorCallExpr* customOperatorCallExpr);
        llvm::Value* generateConstructTemporaryValueExpr(const ConstructTemporaryValueExpr* constructTemporaryValueExpr);
        llvm::Value* generateCustomPrefixOperatorCallExpr(const CustomPrefixOperatorCallExpr* customPrefixOperatorCallExpr);
        llvm::Value* generateCustomCastOperatorCallExpr(const CustomCastOperatorCallExpr* customCastOperatorCallExpr);
        llvm::Value* generateCustomIndexOperatorCallExpr(const CustomIndexOperatorCallExpr* customIndexOperatorCallExpr);
        llvm::Value* generateCustomCallOperatorCallExpr(const CustomCallOperatorCallExpr* customCallOperatorCallExpr);
        llvm::Value* generateCustomPostfixOperatorCallExpr(const CustomPostfixOperatorCallExpr* customPostfixOperatorCallExpr);

        void generateBaseConstructorCallExpr(const BaseConstructorCallExpr* baseConstructorCallExpr);
        void generateBaseDestructorCallExpr(const BaseDestructorCallExpr* baseDestructorCallExpr);

        void castValue(gulc::Type* to, gulc::Type* from, llvm::Value*& value,
                       TextPosition const& startPosition, TextPosition const& endPosition);

        // Context info
        FileAST* currentFileAst;
        const NamespaceDecl* currentNamespace;
        const StructDecl* currentStruct;
        llvm::LLVMContext* llvmContext;
        llvm::IRBuilder<>* irBuilder;
        llvm::Module* module;
        llvm::legacy::FunctionPassManager* funcPass;
        unsigned int loopNameNumber;

        llvm::Function* currentFunction;
        std::vector<llvm::AllocaInst*> currentFunctionParameters;
        llvm::IRBuilder<>* entryBlockBuilder;
        std::map<std::string, llvm::BasicBlock*> currentFunctionLabels;
        unsigned int currentFunctionLocalVariablesCount;
        std::vector<llvm::AllocaInst*> currentFunctionLocalVariables;

        llvm::BasicBlock* currentLoopBlockContinue;
        llvm::BasicBlock* currentLoopBlockBreak;

        unsigned int nestedLoopCount;
        std::vector<llvm::BasicBlock*> nestedLoopContinues;
        std::vector<llvm::BasicBlock*> nestedLoopBreaks;

        std::map<std::string, llvm::StructType*> llvmStructTypes;

        // These are the results of any function calls or related. These need to be checked for if they need destructed
        std::vector<TemporaryValue> temporaryValues;

        bool currentFunctionLabelsContains(const std::string& labelName) {
            return currentFunctionLabels.find(labelName) != currentFunctionLabels.end();
        }

        void addCurrentFunctionLabel(const std::string& labelName, llvm::BasicBlock* basicBlock) {
            if (currentFunctionLabels.find(labelName) == currentFunctionLabels.end()) {
                currentFunctionLabels.insert({labelName, basicBlock});
            }
        }

        void setCurrentFunction(llvm::Function* currentFunction) {
            if (entryBlockBuilder) {
                delete entryBlockBuilder;
                entryBlockBuilder = nullptr;
            }

            currentFunctionParameters.clear();
            currentFunctionLocalVariables.clear();
            currentFunctionLabels.clear();

            this->currentFunction = currentFunction;
            this->entryBlockBuilder = new llvm::IRBuilder<>(&currentFunction->getEntryBlock(),
                                                            currentFunction->getEntryBlock().begin());

            for (llvm::Argument& arg : currentFunction->args()) {
                llvm::AllocaInst* allocaInst = this->entryBlockBuilder->CreateAlloca(arg.getType(), nullptr);

                currentFunctionParameters.push_back(allocaInst);

                this->entryBlockBuilder->CreateStore(&arg, allocaInst);
            }
        }

        llvm::AllocaInst* addLocalVariable(const std::string& varName, llvm::Type* llvmType) {
            llvm::AllocaInst* allocaInst = this->irBuilder->CreateAlloca(llvmType, nullptr, varName);

            if (currentFunctionLocalVariablesCount >= currentFunctionLocalVariables.size()) {
                currentFunctionLocalVariables.push_back(allocaInst);
            } else {
                currentFunctionLocalVariables[currentFunctionLocalVariablesCount] = allocaInst;
            }

            ++currentFunctionLocalVariablesCount;

            return allocaInst;
        }

        llvm::AllocaInst* getLocalVariableOrNull(const std::string& varName) {
            for (std::size_t i = 0; i < currentFunctionLocalVariablesCount; ++i) {
                std::string test = currentFunctionLocalVariables[i]->getName();

                if (currentFunctionLocalVariables[i]->getName() == varName) {
                    return currentFunctionLocalVariables[i];
                }
            }

            return nullptr;
        }

        void enterNestedLoop(llvm::BasicBlock* continueLoop, llvm::BasicBlock* breakLoop) {
            if (nestedLoopCount >= nestedLoopContinues.size()) {
                nestedLoopContinues.push_back(continueLoop);
                nestedLoopBreaks.push_back(breakLoop);
            } else {
                nestedLoopContinues[nestedLoopCount] = continueLoop;
                nestedLoopBreaks[nestedLoopCount] = breakLoop;
            }

            ++nestedLoopCount;
        }

        void leaveNestedLoop() {
            --nestedLoopCount;
        }

        llvm::BasicBlock* getBreakBlock(const std::string& blockName) {
            std::string breakBlockName = blockName + "_break";

            for (std::size_t i = 0; i < nestedLoopCount; ++i) {
                if (nestedLoopBreaks[i]->getName() == breakBlockName) {
                    return nestedLoopBreaks[i];
                }
            }

            return nullptr;
        }

        llvm::BasicBlock* getContinueBlock(const std::string& blockName) {
            std::string continueBlockName = blockName + "_continue";

            for (std::size_t i = 0; i < nestedLoopCount; ++i) {
                if (nestedLoopContinues[i]->getName() == continueBlockName) {
                    return nestedLoopContinues[i];
                }
            }

            return nullptr;
        }

        llvm::StructType* getLlvmStructType(const StructDecl* structDecl, bool unpadded = false) {
            if (unpadded && llvmStructTypes.find(structDecl->mangledName() + ".unpadded") != llvmStructTypes.end()) {
                return llvmStructTypes[structDecl->mangledName() + ".unpadded"];
            } else if (!unpadded && llvmStructTypes.find(structDecl->mangledName()) != llvmStructTypes.end()) {
                return llvmStructTypes[structDecl->mangledName()];
            } else {
                // TODO: Support `[packed]` attribute...
                std::vector<llvm::Type*> elements;
                std::vector<llvm::Type*> elementsPadded;

                if (structDecl->baseStruct != nullptr) {
                    llvm::Type* baseType = getLlvmStructType(structDecl->baseStruct, true);

                    elements.push_back(baseType);
                    elementsPadded.push_back(baseType);
                }

                for (GlobalVariableDecl* dataMember : structDecl->dataMembers) {
                    llvm::Type* memberType = generateLlvmType(dataMember->type);

                    elements.push_back(memberType);
                    elementsPadded.push_back(memberType);
                }

                std::size_t structAlign = genTarget->alignofStruct();
                std::size_t alignPadding = 0;

                // `align` can't be zero, `n % 0` is illegal since `n / 0` is illegal
                if (structAlign != 0) {
                    alignPadding = structAlign - (structDecl->completeSizeWithoutPad % structAlign);

                    // Rather than deal with casting to a signed type and rearrange the above algorithm to prevent
                    // this from happening, we just check if the `alignPadding` is equal to the `align` and set
                    // `alignPadding` to zero if it happens
                    if (alignPadding == structAlign) {
                        alignPadding = 0;
                    }
                }

                // If the `alignPadding` isn't zero then we pad the end of the struct
                if (alignPadding > 0) {
                    elementsPadded.push_back(llvm::ArrayType::get(llvm::Type::getInt8Ty(*llvmContext),
                                                                  alignPadding));
                }

                auto result = llvm::StructType::create(*llvmContext, elements,
                                                       structDecl->name() + ".unpadded", true);
                auto resultPadded = llvm::StructType::create(*llvmContext, elementsPadded, structDecl->name(),
                                                             true);
                llvmStructTypes.insert({structDecl->mangledName() + ".unpadded", result});
                llvmStructTypes.insert({structDecl->mangledName(), resultPadded});

                if (unpadded) {
                    return result;
                } else {
                    return resultPadded;
                }
            }
        }

        llvm::FunctionType* getFunctionType(FunctionDecl* functionDecl) {
            std::vector<llvm::Type*> paramTypes = generateParamTypes(functionDecl->parameters,
                                                                     functionDecl->parentStruct);
            llvm::Type* returnType = generateLlvmType(functionDecl->resultType);

            return llvm::FunctionType::get(returnType, paramTypes, false);
        }

        llvm::Function* getFunction(FunctionDecl* functionDecl) {
            std::vector<llvm::Type*> paramTypes = generateParamTypes(functionDecl->parameters,
                                                                     functionDecl->parentStruct);
            llvm::Type* returnType = generateLlvmType(functionDecl->resultType);
            llvm::FunctionType* functionType = llvm::FunctionType::get(returnType, paramTypes, false);

            // TODO: Why does this return `llvm::Constant*` instead of `llvm::Function*`?
            return llvm::dyn_cast<llvm::Function>(module->getOrInsertFunction(functionDecl->mangledName(), functionType));
        }

        /// Takes an `llvm::Value` and stores it in an `Alloca`, making it a temporary value.
        llvm::AllocaInst* makeTemporaryValue(gulc::Type* type, llvm::Value* value) {
            llvm::AllocaInst* allocaInst = this->irBuilder->CreateAlloca(generateLlvmType(type));

            // TODO: I'm adding `isVolatile` here because one of the IR passes is changing our single store
            //       into an individual store for every single member of the struct. Not sure if this is correct or not
//            irBuilder->CreateStore(value, allocaInst, true);
            irBuilder->CreateStore(value, allocaInst);

            temporaryValues.emplace_back(TemporaryValue(type, allocaInst));

            return allocaInst;
        }

        /// Loop the temporary values, destruct them if needed, and clear the list
        void cleanupTemporaryValues() {
            for (TemporaryValue& temporaryValue : temporaryValues) {
                if (llvm::isa<StructType>(temporaryValue.gulType)) {
                    auto structDecl = llvm::dyn_cast<StructType>(temporaryValue.gulType)->decl();

                    // Because we only work on by value structs we don't have to deal with the vtable, we can know the
                    // struct is the correct type.
                    // TODO: Is this a correct assumption?
                    auto destructorFunc = module->getFunction(structDecl->destructor->mangledName());

                    std::vector<llvm::Value*> llvmArgs {
                            // We pass the struct reference as the first argument of the destructor
                            // the destructor doesn't return anything. It modifies the `this` variable that we pass here
                            temporaryValue.llvmReference
                    };

                    // Call the destructor...
                    // We don't bother giving the result a name since destructors return void
                    irBuilder->CreateCall(destructorFunc, llvmArgs);
                }
            }

            temporaryValues.clear();
        }

    };
}

#endif //GULC_CODEGEN_HPP
