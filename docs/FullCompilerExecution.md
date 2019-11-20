# Full Compiler Execution
This document tries to document the proper way to go from parsing in the source file to outputting the fully linked object file and any other information needed along the way.

NOTE: This document DOES NOT explain how the current implementation of the compiler handles compiling. It is a working document of what I have learned while making the current compiler and ways to fix the current compiler or potentially ways to properly implement a future, bootstrapped compiler.
A plan I have thought about for a while now has been to get the current compiler working as close to perfect as possible so that I can port sections and reimplement other sections into a purely GUL compiler.

## Parsing
Parsing handles taking in the source code file and converting it to a rough AST without anything resolved. All types, attributes, function calls, etc. are all unresolved versions of themselves.
Parsing should handle storing the start and stop locations of every parsed Decl, Stmt, Expr, etc.

### AST
Creating an AST in the way described below is needed to be able to properly handle both compilation, error message generation, and IDE needs.

#### Decls
Decls are ALL top level declarations:
 * ConstructorDecl
 * DestructorDecl
 * EnumDecl
 * FunctionDecl
 * VariableDecl
 * NamespaceDecl
 * StructDecl
 * TraitDecl
 * TemplateFunctionDecl
 * TemplateStructDecl
 * TemplateTraitDecl

Every Decl MUST store the visibility (`public`, `private`, `protected`, `internal`, `protected internal`) regardless of whether the visibility is valid in that context.

Every Decl MUST store a list of declared attributes (`[pod]`, `[deprecated]`, UnresolvedAttribute, etc.) regardless of whether the attribute is valid in its context.

Every Decl MUST store locations for each of its components.

`public class Example {}` must store the location of `public`, `class`, `Example`, `{}`, AND the default `startPosition` and `endPosition` MUST be the start for `public` and the end for `Example` it MUST NOT contain the `{}` and MUST NOT be the start position of the first attribute.

Example:
    
    [pod]
    public struct vec2i
    {
        public i32 x;
        public i32 y;
        
        public vec2i()
        {
            this.x = 0;
            this.y = 0;
        }
    }
    
After parsing this example, the `StructDecl.startPosition` MUST equal the start position of `public` and the `StructDecl.endPosition` MUST equal the end position of `vec2i`.
`StructDecl.attributes[0].startPosition` MUST equal the start position of `[pod]`.
You HAVE TO contain the start for `{` and the end for `}`

#### Stmts
Stmts are all AST nodes that are parsed within a `CompoundStmt` (which is itself contained by `ConstructorDecl`, `DestructorDecl`, `FunctionDecl`, etc.):
 * BreakStmt
 * CompoundStmt
 * ContinueStmt
 * DoStmt
 * ForStmt
 * GotoStmt
 * IfStmt
 * LabeledStmt
 * MacroStmt (e.g. `foreach! (int i in enumerableVar)`)
 * ReturnStmt
 * SwitchStmt
 * ThrowStmt
 * TryStmt
 * WhileStmt

Stmts MUST store a list of declared attributes (i.e. `[volatile]`, custom attributes, etc.).

Stmts MUST store locations for each of its components.

Example:
    
    [nounroll]
    for (int i = 0; i < 12; ++i)
    {
        std.io.print("i: {0}", i);
    }
    
In this example we will parse a `ForStmt`. That `ForStmt` MUST contain the start and end location for the `for`, the `(int i = 0;`, the `i < 12;`, and the `++i)`. 
It must also contain a nested Expr that MUST have a start position that equals the `{` and an end position that equals `}`.

`ForStmt.attributes[0].startPosition` MUST equal the start position for `[nounroll]`

#### Exprs
Exprs are all AST nodes that CAN have results (`funcCall();`, `++i`, etc.):
 * BinaryOperatorExpr
 * CharacterLiteralExpr
 * CustomPrefixOperatorCallExpr
 * DestructLocalVariableExpr (might be a statement instead? Technically non-returning but also technically a function call)
 * DestructMemberVariableExpr (might be a statement instead? Technically non-returning but also technically a function call)
 * DestructParameterExpr (might be a statement instead? Technically non-returning but also technically a function call)
 * ExplicitCastExpr
 * FloatLiteralExpr
 * FunctionCallExpr
 * ImplicitCastExpr
 * IndexerCallExpr
 * IntegerLiteralExpr
 * LocalVariableDeclExpr
 * LValueToRValueExpr
 * MemberAccessCallExpr
 * ParenExpr (contains anything within parenthesis, needed for IDEs)
 * PostfixOperatorExpr (cannot be custom)
 * PrefixOperatorExpr
 * RefEnumConstantExpr
 * RefFunctionExpr
 * RefGlobalVariableExpr
 * RefLocalVariableExpr
 * RefParameterExpr
 * RefMemberFunctionExpr
 * RefMemberVariableExpr
 * RefTraitFunctionExpr
 * StringLiteralExpr
 * TernaryExpr

Exprs MUST store the positions for each piece of their expression.

Examples:
    
    ++i; // Stores that start at the beginning of ++ and the end at end of ;, also stores the start and end for just ++
    functionCall(i); // Stores the start and the beginning of 'functionCall' and the end at ';', also stores the locations for the functionCall and the `(`, `)` and `;`
    i + j; // Stores the start and end the same as above, also stores the start and end for '+'
    // etc.

## Compiler Passes
Once the source code of all input files have been parsed and all library's symbols have been loaded in we will have to perform passes on the parsed in AST to resolve types, resolve Decls, do verification, place implicit casts, etc.

Due to how GUL doesn't require declarations to be in order, compiler passes have to be performed to make the AST usable in any way.

### Macro Expansion Pass
NOTE: Does not exist in the current compiler.

The Macro Expansion Pass handles searching for any macros and expanding them into their full form AST. These Macro nodes MUST stay alive within the AST for error messages.

### Type Resolver Pass
The Type Resolver Pass MUST traverse the ENTIRE AST searching for any unresolved type and resolve that type. In the event that the type is not found there should be an error placed in the errors list when parsing for an IDE or the compiler should print that error message and exit.

This pass must also handle resolving base types for structs, creating default constructors, creating default destructors, and removing any ambiguities within the AST that requires a type check for disambiguation (e.g. `Random r;` could either be a local variable named `r` of type `Random` OR a custom prefix operator call to `Random`).

### Decl Resolver Pass
The Decl Resolver Pass MUST traverse the ENTIRE AST searching for anything that can resolve to a Decl.
This pass handles resolving function calls to their `FunctionDecl`, overloaded operator calling, implicit cast placement, variable referencing, etc.

