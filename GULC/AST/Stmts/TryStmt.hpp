#ifndef GULC_TRYSTMT_HPP
#define GULC_TRYSTMT_HPP

#include <AST/Stmt.hpp>
#include <vector>
#include "TryCatchStmt.hpp"
#include "TryFinallyStmt.hpp"
#include "CompoundStmt.hpp"

namespace gulc {
    class TryStmt : public Stmt {
    public:
        static bool classof(const Stmt *stmt) { return stmt->getStmtKind() == Kind::Try; }

        TryStmt(TextPosition startPosition, TextPosition endPosition,
                CompoundStmt* encapsulatedStmt, std::vector<TryCatchStmt*> catchStmts, TryFinallyStmt* finallyStmt)
                : Stmt(Kind::Try, startPosition, endPosition),
                  encapsulatedStmt(encapsulatedStmt), finallyStmt(finallyStmt), _catchStmts(std::move(catchStmts)) {}

        // We require 'CompoundStmt' for 'try', 'catch', and 'finally' to prevent unwanted errors for:
        //
        //     try
        //         // blah...
        //     catch (Exception2 e2)
        //     catch (Exception e)
        //         try
        //             // blah again, use imagination...
        //         catch (IOException ioe)
        //             // blah...
        //         catch (Exception3 e3)
        //
        // In situations similar to that it would be difficult to tell if the nested catch statements are nested
        // or attached to the first try statement
        CompoundStmt* encapsulatedStmt;
        std::vector<TryCatchStmt*>& catchStmts() { return _catchStmts; }
        const std::vector<TryCatchStmt*>& catchStmts() const { return _catchStmts; }
        TryFinallyStmt* finallyStmt;

        bool hasCatchStmts() const { return !_catchStmts.empty(); }
        bool hasFinallyStmt() const { return finallyStmt != nullptr; }

        ~TryStmt() override {
            for (TryCatchStmt* tryCatchStmt : _catchStmts) {
                delete tryCatchStmt;
            }

            delete encapsulatedStmt;
            delete finallyStmt;
        }

    private:
        std::vector<TryCatchStmt*> _catchStmts;

    };
}

#endif //GULC_TRYSTMT_HPP
