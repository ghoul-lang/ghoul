#ifndef GULC_TYPE_HPP
#define GULC_TYPE_HPP

#include <MetaData/TextPosition.hpp>
#include <string>

namespace gulc {
    class Type {
    public:
        enum class Kind {
            TemplateTypename,
            Unresolved,
            FunctionTemplateTypenameRef,
            BuiltIn,
            Pointer,
            FunctionPointer,
            Const,
            Mut,
            Immut
        };

        Kind getTypeKind() const { return _kind; }
        TextPosition startPosition() const { return _startPosition; }
        TextPosition endPosition() const { return _endPosition; }
        virtual std::string getString() const = 0;

        virtual ~Type() = default;

    protected:
        Type(Kind kind, TextPosition startPosition, TextPosition endPosition)
                : _kind(kind), _startPosition(startPosition), _endPosition(endPosition) {}

    private:
        const Kind _kind;
        const TextPosition _startPosition;
        const TextPosition _endPosition;

    };
}

#endif //GULC_TYPE_HPP
