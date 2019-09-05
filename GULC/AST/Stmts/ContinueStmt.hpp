#ifndef GULC_CONTINUESTMT_HPP
#define GULC_CONTINUESTMT_HPP

#include <AST/Stmt.hpp>
#include <string>

namespace gulc {
    class ContinueStmt : public Stmt {
    public:
        static bool classof(const Stmt *stmt) { return stmt->getStmtKind() == StmtKind::Continue; }

        ContinueStmt(TextPosition startPosition, TextPosition endPosition, std::string label)
                : Stmt(StmtKind::Continue, startPosition, endPosition),
                  _label(std::move(label)) {}

        std::string label() const { return _label; }

    private:
        std::string _label;

    };
}

#endif //GULC_CONTINUESTMT_HPP
