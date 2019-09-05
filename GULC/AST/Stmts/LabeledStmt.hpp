#ifndef GULC_LABELEDSTMT_HPP
#define GULC_LABELEDSTMT_HPP

#include <AST/Stmt.hpp>
#include <string>

namespace gulc {
    class LabeledStmt : public Stmt {
    public:
        static bool classof(const Stmt *stmt) { return stmt->getStmtKind() == StmtKind::Labeled; }

        LabeledStmt(TextPosition startPosition, TextPosition endPosition, std::string label, Stmt* labeledStmt)
                : Stmt(StmtKind::Labeled, startPosition, endPosition),
                  _label(std::move(label)), _labeledStmt(labeledStmt) {}

        std::string label() const { return _label; }
        const Stmt* labeledStmt() const { return _labeledStmt; }

        ~LabeledStmt() override {
            delete _labeledStmt;
        }

    private:
        std::string _label;
        Stmt* _labeledStmt;
    };
}

#endif //GULC_LABELEDSTMT_HPP
