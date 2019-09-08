#ifndef GULC_COMPOUNDSTMT_HPP
#define GULC_COMPOUNDSTMT_HPP

#include <AST/Stmt.hpp>
#include <vector>

namespace gulc {
    class CompoundStmt : public Stmt {
    public:
        static bool classof(const Stmt *stmt) { return stmt->getStmtKind() == Kind::Compound; }

        CompoundStmt(TextPosition startPosition, TextPosition endPosition, std::vector<Stmt*> statements)
                : Stmt(Kind::Compound, startPosition, endPosition), _statements(std::move(statements)) {}

        const std::vector<Stmt*>& statements() const { return _statements; }
        std::vector<Stmt*>& statements() { return _statements; }

    private:
        std::vector<Stmt*> _statements;

    };
}

#endif //GULC_COMPOUNDSTMT_HPP
