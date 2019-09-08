#ifndef GULC_BREAKSTMT_HPP
#define GULC_BREAKSTMT_HPP

#include <AST/Stmt.hpp>
#include <string>

namespace gulc {
    class BreakStmt : public Stmt {
    public:
        static bool classof(const Stmt *stmt) { return stmt->getStmtKind() == Kind::Break; }

        BreakStmt(TextPosition startPosition, TextPosition endPosition, std::string label)
                : Stmt(Kind::Break, startPosition, endPosition),
                  _label(std::move(label)) {}

        std::string label() const { return _label; }

    private:
        std::string _label;

    };
}

#endif //GULC_BREAKSTMT_HPP
