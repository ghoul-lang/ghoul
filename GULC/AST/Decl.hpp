#ifndef GULC_DECL_HPP
#define GULC_DECL_HPP

#include <string>
#include <MetaData/TextPosition.hpp>
#include "llvm/Support/Casting.h"

namespace gulc {
    class Decl {
    public:
        enum class Kind {
            Function,
            // TODO: Add 'MemberFunctionDecl' (a.k.a. MethodDecl) and 'MemberPropertyDecl' (which is why we call it 'MemberFunctionDecl' instead of 'MethodDecl')
            // TODO: Create 'Property'
            Property,
            MemberFunction,
            MemberProperty,
            Parameter,
            TemplateParameterDecl
        };

        Kind getDeclKind() const { return _kind; }
        std::string sourceFile() const { return _sourceFile; }
        TextPosition startPosition() const { return _startPosition; }
        TextPosition endPosition() const { return _endPosition; }

        virtual ~Decl() = default;

    protected:
        Decl(Kind kind, std::string sourceFile, TextPosition startPosition, TextPosition endPosition)
                : _kind(kind),
                  _sourceFile(std::move(sourceFile)), _startPosition(startPosition), _endPosition(endPosition) {}

    private:
        const Kind _kind;
        const std::string _sourceFile;
        const TextPosition _startPosition;
        const TextPosition _endPosition;

    };
}

#endif //GULC_DECL_HPP
