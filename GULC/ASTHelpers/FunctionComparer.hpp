#ifndef GULC_FUNCTIONCOMPARER_HPP
#define GULC_FUNCTIONCOMPARER_HPP

#include <AST/Decls/FunctionDecl.hpp>
#include <AST/Decls/TemplateFunctionDecl.hpp>

namespace gulc {

    class FunctionComparer {
    public:
        enum class CompareResult {
            /// Functions have the same name and argument lengths and types
            /// e.x. `int test(int i)` and `void test(int j)`
            ///      `int test(int j)` and `int test(const int i)`
            Identical,
            /// Functions have the same name and the same arguments except one has optional arguments
            /// e.x. `int test(int i, int j)` and `int test(int i, int j, int k = 12)`
            SimilarDifferentiable,
            Different
        };

        static CompareResult compareParams(const std::vector<ParameterDecl*>& originalParams,
                                           const std::vector<ParameterDecl*>& compareToParams);

        /**
         * Compare two functions to see how similar they are
         *
         * @param original The original function that is being compared
         * @param compareTo The function to compare the original function to
         * @return How similar the two functions are
         */
        static CompareResult compare(const FunctionDecl* original, const FunctionDecl* compareTo);

        /**
         * Compare two template functions to see how similar they are
         *
         * @param original The original template function that is being compared
         * @param compareTo The template function to compare the original template function to
         * @return How similar the two template functions are
         */
        static CompareResult compare(const TemplateFunctionDecl* original, const TemplateFunctionDecl* compareTo);

    };
}

#endif //GULC_FUNCTIONCOMPARER_HPP
