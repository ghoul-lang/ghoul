#include <iostream>
#include "ASTPrinter.hpp"

using namespace gulc;

void ASTPrinter::printDecl(const Decl *decl, const std::string& prefix) {
    switch (decl->getDeclKind()) {
        case Decl::DeclKind::Function:
            printFunctionDecl(llvm::dyn_cast<FunctionDecl>(decl), prefix);
            break;
        default:
            std::cout << prefix << "[UNSUPPORTED DECL]" << std::endl;
            break;
    }
}

void ASTPrinter::printStmt(const Stmt *stmt, const std::string& prefix) {
    switch (stmt->getStmtKind()) {
        case Stmt::StmtKind::Compound:
            printCompoundStmt(llvm::dyn_cast<CompoundStmt>(stmt), prefix);
            break;
        case Stmt::StmtKind::Return:
            printReturnStmt(llvm::dyn_cast<ReturnStmt>(stmt), prefix);
            break;
        default:
            std::cout << prefix << "[UNSUPPORTED STMT]" << std::endl;
            break;
    }
}

void ASTPrinter::printExpr(const Expr *expr, const std::string& prefix) {
    switch (expr->getExprKind()) {
        case Expr::ExprKind::BinaryOperator:
            printBinaryOperatorExpr(llvm::dyn_cast<BinaryOperatorExpr>(expr), prefix);
            break;
        case Expr::ExprKind::Identifier:
            printIdentifierExpr(llvm::dyn_cast<IdentifierExpr>(expr), prefix);
            break;
        case Expr::ExprKind::IntegerLiteral:
            printIntegerLiteralExpr(llvm::dyn_cast<IntegerLiteralExpr>(expr), prefix);
            break;
        case Expr::ExprKind::FunctionCall:
            printFunctionCallExpr(llvm::dyn_cast<FunctionCallExpr>(expr), prefix);
            break;
        case Expr::ExprKind::PrefixOperator:
            printPrefixOperatorExpr(llvm::dyn_cast<PrefixOperatorExpr>(expr), prefix);
            break;
        case Expr::ExprKind::PostfixOperator:
            printPostfixOperatorExpr(llvm::dyn_cast<PostfixOperatorExpr>(expr), prefix);
            break;
        case Expr::ExprKind::Paren:
            printParenExpr(llvm::dyn_cast<ParenExpr>(expr), prefix);
            break;
        case Expr::ExprKind::FloatLiteral:
            printFloatLiteralExpr(llvm::dyn_cast<FloatLiteralExpr>(expr), prefix);
            break;
        case Expr::ExprKind::IndexerCall:
        case Expr::ExprKind::MemberAccessCall:
        case Expr::ExprKind::Ternary:
        default:
            std::cout << prefix << "[UNSUPPORTED EXPR]" << std::endl;
            break;
    }
}

// Types
std::string ASTPrinter::getTypeName(const Type *type) {
    switch (type->getTypeKind()) {
        case Type::TypeKind::Unresolved: {
            // TODO: We should also append the namespace
            return llvm::dyn_cast<UnresolvedType>(type)->name();
        }
        default: {
            return "UNKNOWN TYPE";
        }
    }
}

// Decls
std::string ASTPrinter::getParametersString(const std::vector<ParameterDecl *> &parameters) {
    std::string result;

    bool isFirst = true;

    for (const ParameterDecl* parameterDecl : parameters) {
        if (isFirst) {
            isFirst = false;
        } else {
            result += ", ";
        }

        result += getTypeName(parameterDecl->type()) + " " + parameterDecl->name();
    }

    return result;
}

void ASTPrinter::printFunctionDecl(const FunctionDecl *functionDecl, const std::string& prefix) {
    std::cout << prefix << "| FunctionDecl `" << functionDecl->name() << "`(" << getParametersString(functionDecl->parameters()) << ") "
                           "-> " << getTypeName(functionDecl->resultType())
              << std::endl;
    printCompoundStmt(functionDecl->body(), prefix + "  ");
}

// Stmts
void ASTPrinter::printCompoundStmt(const CompoundStmt *compoundStmt, const std::string& prefix) {
    std::cout << prefix << "| CompoundStmt " << std::endl;

    for (Stmt* stmt : compoundStmt->statements()) {
        printStmt(stmt, prefix + "  ");
    }
}

void ASTPrinter::printReturnStmt(const ReturnStmt *returnStmt, const std::string& prefix) {
    std::cout << prefix << "| ReturnStmt ";

    if (returnStmt->returnValue() == nullptr) {
        std::cout << "(Empty)" << std::endl;
        return;
    }

    std::cout << std::endl;

    printExpr(returnStmt->returnValue(), prefix + "  ");
}

// Exprs
void ASTPrinter::printBinaryOperatorExpr(const BinaryOperatorExpr *binaryOperatorExpr, const std::string &prefix) {
    std::cout << prefix << "| BinaryOperatorExpr (op: '" << binaryOperatorExpr->operatorName() << "') " << std::endl;
    std::cout << prefix << "\\ LeftValue:" << std::endl;
    printExpr(binaryOperatorExpr->leftValue(), prefix + "  ");
    std::cout << prefix << "\\ RightValue:" << std::endl;
    printExpr(binaryOperatorExpr->rightValue(), prefix + "  ");
}

void ASTPrinter::printIdentifierExpr(const IdentifierExpr *identifierExpr, const std::string &prefix) {
    // TODO: Need to support the template args
    std::cout << prefix << "| IdentifierExpr (name: " << identifierExpr->name() << ")" << std::endl;
}

void ASTPrinter::printIntegerLiteralExpr(const IntegerLiteralExpr *integerLiteralExpr, const std::string &prefix) {
    std::cout << prefix << "| IntegerLiteralExpr (base: " << integerLiteralExpr->numberBase() << ", number: " << integerLiteralExpr->numberString() << ") " << std::endl;
}

void ASTPrinter::printFunctionCallExpr(const FunctionCallExpr *functionCallExpr, const std::string &prefix) {
    std::cout << prefix << "| FunctionCallExpr " << std::endl;
    std::cout << prefix << "\\ FunctionReference: " << std::endl;
    printExpr(functionCallExpr->functionReference(), prefix + "  ");
    std::cout << prefix << "\\ Arguments: " << std::endl;

    for (const Expr* argument : functionCallExpr->arguments()) {
        printExpr(argument, prefix + "  ");
    }
}

void ASTPrinter::printPrefixOperatorExpr(const PrefixOperatorExpr *prefixOperatorExpr, const std::string &prefix) {
    std::cout << prefix << "| PrefixOperatorExpr (op: '" << prefixOperatorExpr->operatorName() << "')" << std::endl;
    printExpr(prefixOperatorExpr->expr(), prefix + "  ");
}

void ASTPrinter::printPostfixOperatorExpr(const PostfixOperatorExpr *postfixOperatorExpr, const std::string &prefix) {
    std::cout << prefix << "| PostfixOperatorExpr (op: '" << postfixOperatorExpr->operatorName() << "')" << std::endl;
    printExpr(postfixOperatorExpr->expr(), prefix + "  ");
}

void ASTPrinter::printParenExpr(const ParenExpr *parenExpr, const std::string &prefix) {
    std::cout << prefix << "| ParenExpr " << std::endl;
    printExpr(parenExpr->containedExpr(), prefix + "  ");
}

void ASTPrinter::printFloatLiteralExpr(const FloatLiteralExpr *floatLiteralExpr, const std::string &prefix) {
    std::cout << prefix << "| FloatLiteralExpr (number: " << floatLiteralExpr->numberValue() << ")" << std::endl;
}
