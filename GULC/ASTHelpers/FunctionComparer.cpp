#include <AST/Types/ReferenceType.hpp>
#include "FunctionComparer.hpp"

using namespace gulc;

FunctionComparer::CompareResult FunctionComparer::compare(const FunctionDecl *original,
                                                          const FunctionDecl *compareTo) {
    // If they have different names then they are always different
    if (original->name() != compareTo->name()) {
        return CompareResult::Different;
    }

    std::size_t originalSize = original->parameters.size();
    std::size_t compareToSize = compareTo->parameters.size();
    std::size_t paramSize = originalSize;

    if (compareToSize > paramSize) {
        paramSize = compareToSize;
    }

    for (std::size_t i = 0; i < paramSize; ++i) {
        if (i >= originalSize) {
            // If we've reached the end of the original's parameters...
            if (compareTo->parameters[i]->hasDefaultArgument()) {
                // If the compare to parameter has a default value then the two functions are similar but differentiable
                return CompareResult::SimilarDifferentiable;
            } else {
                // If the compare to parameter does not have a default value then the two functions are different
                return CompareResult::Different;
            }
        } else if (i >= compareToSize) {
            // If we've reached the end of the compare to's parameters...
            if (original->parameters[i]->hasDefaultArgument()) {
                // If the original parameter has a default value then the two functions are similar but differentiable
                return CompareResult::SimilarDifferentiable;
            } else {
                // If the compare to parameter does not have a default value then the two functions are different
                return CompareResult::Different;
            }
        } else if (original->parameters[i]->hasDefaultArgument() != compareTo->parameters[i]->hasDefaultArgument()) {
            // If one of the parameters is default and the other isn't then the two functions are similar but
            // differentiable
            return CompareResult::SimilarDifferentiable;
        } else {
            bool ignoreFirstQualifier = false;
            Type* originalParam = original->parameters[i]->type;
            Type* compareToParam = compareTo->parameters[i]->type;

            bool originalParamIsRef = false;

            // If either of the parameters are references we remove the reference as we only care
            // about the underlying type.
            // This also works if BOTH functions have reference types because:
            // `int&` and `int&` is a shadow and we return false
            // `int mut&` and `int&` are two unique functions and we keep looking
            // `int&` and `int` is a shadow so we return false
            if (llvm::isa<ReferenceType>(originalParam)) {
                originalParamIsRef = true;

                originalParam = llvm::dyn_cast<ReferenceType>(originalParam)->referenceToType;

                // If compare to param isn't a reference but we are we have to ignore any qualifiers
                if (!llvm::isa<ReferenceType>(compareToParam)) {
                    ignoreFirstQualifier = true;
                }
            }

            if (llvm::isa<ReferenceType>(compareToParam)) {
                compareToParam = llvm::dyn_cast<ReferenceType>(compareToParam)->referenceToType;

                // If original param isn't a reference but we are we have to ignore any qualifiers
                if (!originalParamIsRef) {
                    ignoreFirstQualifier = true;
                }
            }

            // If the params aren't the same type then the functions aren't the same
            if (!TypeComparer::getTypesAreSame(originalParam, compareToParam, ignoreFirstQualifier)) {
                return CompareResult::Different;
            }
        }
    }

    return CompareResult::Identical;
}

FunctionComparer::CompareResult FunctionComparer::compare(const TemplateFunctionDecl *original,
                                                          const TemplateFunctionDecl *compareTo) {
    // If they have different names then they are always different
    if (original->name() != compareTo->name()) {
        return CompareResult::Different;
    }

    std::size_t originalTemplateSize = original->templateParameters.size();
    std::size_t compareToTemplateSize = compareTo->templateParameters.size();
    std::size_t templateParamSize = originalTemplateSize;

    if (compareToTemplateSize > originalTemplateSize) {
        templateParamSize = compareToTemplateSize;
    }

    for (std::size_t i = 0; i < templateParamSize; ++i) {
        if (i >= originalTemplateSize) {
            // If we've reached the end of the original's template parameters...
            if (compareTo->templateParameters[i]->hasDefaultArgument()) {
                // If the compare to parameter has a default value then the two functions are similar but differentiable
                return CompareResult::SimilarDifferentiable;
            } else {
                // If the compare to parameter does not have a default value then the two functions are different
                return CompareResult::Different;
            }
        } else if (i >= compareToTemplateSize) {
            // If we've reached the end of the compare to's template parameters...
            if (original->templateParameters[i]->hasDefaultArgument()) {
                // If the original parameter has a default value then the two functions are similar but differentiable
                return CompareResult::SimilarDifferentiable;
            } else {
                // If the compare to parameter does not have a default value then the two functions are different
                return CompareResult::Different;
            }
        } else if (original->templateParameters[i]->hasDefaultArgument() !=
                   compareTo->templateParameters[i]->hasDefaultArgument()) {
            // If one of the template parameters is default and the other isn't then the two functions are similar but
            // differentiable
            return CompareResult::SimilarDifferentiable;
        } else {
            bool ignoreFirstQualifier = false;
            Type* originalParam = original->templateParameters[i]->type;
            Type* compareToParam = compareTo->templateParameters[i]->type;

            if (llvm::isa<TemplateTypenameType>(originalParam) ==
                !llvm::isa<TemplateTypenameType>(compareToParam)) {
                // If one is a `typename` and the other isn't then we can differentiate between the two
                // functions
                return CompareResult::Different;
            }

            bool structParamIsRef = false;

            // If either of the parameters are references we remove the reference as we only care
            // about the underlying type.
            // This also works if BOTH functions have refrence types because:
            // `int&` and `int&` is a shadow and we return false
            // `int mut&` and `int&` are two unique functions and we keep looking
            // `int&` and `int` is a shadow so we return false
            if (llvm::isa<ReferenceType>(originalParam)) {
                structParamIsRef = true;

                originalParam = llvm::dyn_cast<ReferenceType>(originalParam)->referenceToType;

                // If compare template to param isn't a reference but we are we have to ignore any qualifiers
                if (!llvm::isa<ReferenceType>(compareToParam)) {
                    ignoreFirstQualifier = true;
                }
            }

            if (llvm::isa<ReferenceType>(compareToParam)) {
                compareToParam = llvm::dyn_cast<ReferenceType>(compareToParam)->referenceToType;

                // If original template param isn't a reference but we are we have to ignore any qualifiers
                if (!structParamIsRef) {
                    ignoreFirstQualifier = true;
                }
            }

            // If the params aren't the same type then the functions aren't the same
            if (!TypeComparer::getTypesAreSame(originalParam, compareToParam, ignoreFirstQualifier)) {
                return CompareResult::Different;
            }
        }
    }

    std::size_t originalSize = original->parameters.size();
    std::size_t compareToSize = compareTo->parameters.size();
    std::size_t paramSize = originalSize;

    if (compareToSize > paramSize) {
        paramSize = compareToSize;
    }

    for (std::size_t i = 0; i < paramSize; ++i) {
        if (i >= originalSize) {
            // If we've reached the end of the original's parameters...
            if (compareTo->parameters[i]->hasDefaultArgument()) {
                // If the compare to parameter has a default value then the two functions are similar but differentiable
                return CompareResult::SimilarDifferentiable;
            } else {
                // If the compare to parameter does not have a default value then the two functions are different
                return CompareResult::Different;
            }
        } else if (i >= compareToSize) {
            // If we've reached the end of the compare to's parameters...
            if (original->parameters[i]->hasDefaultArgument()) {
                // If the original parameter has a default value then the two functions are similar but differentiable
                return CompareResult::SimilarDifferentiable;
            } else {
                // If the compare to parameter does not have a default value then the two functions are different
                return CompareResult::Different;
            }
        } else if (original->parameters[i]->hasDefaultArgument() != compareTo->parameters[i]->hasDefaultArgument()) {
            // If one of the parameters is default and the other isn't then the two functions are similar but
            // differentiable
            return CompareResult::SimilarDifferentiable;
        } else {
            bool ignoreFirstQualifier = false;
            Type* originalParam = original->parameters[i]->type;
            Type* compareToParam = compareTo->parameters[i]->type;

            bool originalParamIsRef = false;

            // If either of the parameters are references we remove the reference as we only care
            // about the underlying type.
            // This also works if BOTH functions have reference types because:
            // `int&` and `int&` is a shadow and we return false
            // `int mut&` and `int&` are two unique functions and we keep looking
            // `int&` and `int` is a shadow so we return false
            if (llvm::isa<ReferenceType>(originalParam)) {
                originalParamIsRef = true;

                originalParam = llvm::dyn_cast<ReferenceType>(originalParam)->referenceToType;

                // If compare to param isn't a reference but we are we have to ignore any qualifiers
                if (!llvm::isa<ReferenceType>(compareToParam)) {
                    ignoreFirstQualifier = true;
                }
            }

            if (llvm::isa<ReferenceType>(compareToParam)) {
                compareToParam = llvm::dyn_cast<ReferenceType>(compareToParam)->referenceToType;

                // If original param isn't a reference but we are we have to ignore any qualifiers
                if (!originalParamIsRef) {
                    ignoreFirstQualifier = true;
                }
            }

            // If the params aren't the same type then the functions aren't the same
            if (!TypeComparer::getTypesAreSame(originalParam, compareToParam, ignoreFirstQualifier)) {
                return CompareResult::Different;
            }
        }
    }

    return CompareResult::Identical;
}
