#ifndef GULC_LABELEDSTMT_HPP
#define GULC_LABELEDSTMT_HPP

#include <AST/Stmt.hpp>
#include <string>

namespace gulc {
    class LabeledStmt : public Stmt {
    public:
        static bool classof(const Stmt *stmt) { return stmt->getStmtKind() == Kind::Labeled; }

        LabeledStmt(TextPosition startPosition, TextPosition endPosition, std::string label, Stmt* labeledStmt)
                : Stmt(Kind::Labeled, startPosition, endPosition),
                  labeledStmt(labeledStmt), _label(std::move(label)) {}

        std::string label() const { return _label; }
        Stmt* labeledStmt;

        ~LabeledStmt() override {
            delete labeledStmt;
        }

    private:
        std::string _label;

    };
}

#endif //GULC_LABELEDSTMT_HPP
