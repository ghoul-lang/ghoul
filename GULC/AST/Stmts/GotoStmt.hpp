#ifndef GULC_GOTOSTMT_HPP
#define GULC_GOTOSTMT_HPP

#include <AST/Stmt.hpp>
#include <string>

namespace gulc {
    class GotoStmt : public Stmt {
    public:
        static bool classof(const Stmt *stmt) { return stmt->getStmtKind() == Kind::Goto; }

        GotoStmt(TextPosition startPosition, TextPosition endPosition, std::string label)
                : Stmt(Kind::Goto, startPosition, endPosition),
                  _label(std::move(label)) {}

        std::string label() const { return _label; }

    private:
        std::string _label;

    };
}

#endif //GULC_GOTOSTMT_HPP
