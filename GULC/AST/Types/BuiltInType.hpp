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
            if (_name == "bool") {
                _isBool = true;
            }

            // TODO: Remove the names that aren't `int#` and `float32`/`float64`
            if (_name == "void") {
                _sizeInBytes = 0;
            } else if (_name == "int8" || _name == "uint8" || _name == "char" || _name == "byte" || _name == "sbyte") {
                _sizeInBytes = 1;
            } else if (_name == "int16" || _name == "uint16" || _name == "float16" || _name == "short" || _name == "ushort") {
                _sizeInBytes = 2;
            } else if (_name == "int32" || _name == "uint32" || _name == "float32" || _name == "int" || _name == "uint" || _name == "float") {
                _sizeInBytes = 4;
            } else if (_name == "int64" || _name == "uint64" || _name == "float64" || _name == "long" || _name == "ulong" || _name == "double") {
                _sizeInBytes = 8;
            } else {
                // Default to 32 bits
                _sizeInBytes = 4;
            }

            // TODO: Remove the `float` and `double`
            if (_name == "float" || _name == "double" || _name == "float32" || _name == "float64") {
                _isFloating = true;
            }

            if (_name == "uint8" || _name == "char" || _name == "byte" ||
                _name == "uint16" || _name == "ushort" ||
                _name == "uint32" || _name == "uint" ||
                _name == "uint64" || _name == "ulong") {
                _isSigned = false;
            }
        }

        std::string name() const { return _name; }
        std::string getString() const override { return _name; }

        unsigned short size() const { return _sizeInBytes; }
        bool isFloating() const { return _isFloating; }
        bool isSigned() const { return _isSigned; }
        bool isBool() const { return _isBool; }

        static bool isBuiltInType(const std::string& name) {
            return name == "bool" || name == "void" ||
                   name == "int8" || name == "uint8" || name == "char" || name == "byte" || name == "sbyte" ||
                   name == "int16" || name == "uint16" || name == "float16" || name == "short" || name == "ushort" ||
                   name == "int32" || name == "uint32" || name == "float32" || name == "int" || name == "uint" || name == "float" ||
                   name == "int64" || name == "uint64" || name == "float64" || name == "long" || name == "ulong" || name == "double";
        }

    private:
        std::string _name;
        unsigned short _sizeInBytes;
        bool _isBool;
        bool _isFloating;
        bool _isSigned;

    };
}

#endif //GULC_BUILTINTYPE_HPP
