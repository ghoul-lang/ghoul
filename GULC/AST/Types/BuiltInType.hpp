#ifndef GULC_BUILTINTYPE_HPP
#define GULC_BUILTINTYPE_HPP

#include <AST/Type.hpp>
#include <string>

namespace gulc {
    class BuiltInType : public Type {
    public:
        static bool classof(const Type *expr) { return expr->getTypeKind() == Kind::BuiltIn; }

        BuiltInType(TextPosition startPosition, TextPosition endPosition, std::string name)
                : Type(Kind::BuiltIn, startPosition, endPosition),
                  _name(std::move(name)), _isBool(false), _isFloating(false), _isSigned(true) {
            if (name == "bool") {
                _isBool = true;
            }

            // TODO: Remove the names that aren't `int#` and `float32`/`float64`
            if (name == "int8" || name == "uint8" || name == "char" || name == "byte" || name == "sbyte") {
                _sizeInBytes = 1;
            } else if (name == "int16" || name == "uint16" || name == "float16" || name == "short" || name == "ushort") {
                _sizeInBytes = 2;
            } else if (name == "int32" || name == "uint32" || name == "float32" || name == "int" || name == "uint" || name == "float") {
                _sizeInBytes = 4;
            } else if (name == "int64" || name == "uint64" || name == "float64" || name == "long" || name == "ulong" || name == "double") {
                _sizeInBytes = 8;
            } else {
                // Default to 32 bits
                _sizeInBytes = 4;
            }

            // TODO: Remove the `float` and `double`
            if (name == "float" || name == "double" || name == "float32" || name == "float64") {
                _isFloating = true;
            }

            if (name == "uint8" || name == "char" || name == "byte" ||
                name == "uint16" || name == "ushort" ||
                name == "uint32" || name == "uint" ||
                name == "uint64" || name == "ulong") {
                _isSigned = false;
            }
        }

        std::string name() const { return _name; }
        std::string getString() const override { return _name; }

        unsigned short size() const { return _sizeInBytes; }
        bool isFloating() const { return _isFloating; }
        bool isSigned() const { return _isSigned; }
        bool isBool() const { return _isBool; }

    private:
        std::string _name;
        unsigned short _sizeInBytes;
        bool _isBool;
        bool _isFloating;
        bool _isSigned;

    };
}

#endif //GULC_BUILTINTYPE_HPP
