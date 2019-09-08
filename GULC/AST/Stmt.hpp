#ifndef GULC_STMT_HPP
#define GULC_STMT_HPP

#include <MetaData/TextPosition.hpp>
#include "llvm/Support/Casting.h"

namespace gulc {
    class Stmt {
    public:
        enum class Kind {
            Compound,
            Expr,
            Return,
            Labeled,

            If,
            While,
            For,
            Do,
            Switch,
            Case,

            Break,
            Continue,
            Goto,

            Try,
            TryCatch,
            TryFinally
        };

        Kind getStmtKind() const { return _kind; }
        TextPosition startPosition() const { return _startPosition; }
        TextPosition endPosition() const { return _endPosition; }

        virtual ~Stmt() = default;

    protected:
        Stmt(Kind kind, TextPosition startPosition, TextPosition endPosition)
                : _kind(kind), _startPosition(startPosition), _endPosition(endPosition) {}

    private:
        const Kind _kind;
        const TextPosition _startPosition;
        const TextPosition _endPosition;

    };
}

#endif //GULC_STMT_HPP
