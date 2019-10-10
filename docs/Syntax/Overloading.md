## Overloading Semantics

In GUL all functions and operators can be overloaded. As long as the type signature is different, two functions can have the same name.

Allowed:

    void test(byte x);
    void test(sbyte x);
    void test(ushort x);
    void test(short x);
    void test(int x);
    void test(uint x);
    void test(long x);
    void test(ulong x);

Because the integer literal `12` with no type suffix is an int32 by default then calling `test(12)` will call `void test(int x)`
If you get rid of the function `void test(int x)` then the call `test(12)` would then be ambiguous, `12` can be implicitly casted to any of the overloaded function types.

### Implicitly Castable Class/Struct

In GUL we allow defining custom implicit cast operators. Because of this we can define a function with the signature `void test(ImplicitExample x)` where `ImplicitExample` has a custom implicit cast function that supports casting from the `ImplicitExample` to `int`
This would make the following allowed:
    
    void test(ImplicitExample x);
    
    test(12);

But if you define another function with the same name with a type that can also be implicitly casted to `int` then the call would be ambiguous:
    
    void test(ImplicitExample x);
    void test(ImplicitExample2 x); // You're allowed to define this function since `ImplicitExample2` is a different type from `ImplicitExample`
    
    test(12); // Error: function call `test` is ambiguous in the current context
    test(ImplicitExample(12)); // OK
    test(ImplicitExample2(12)); // OK

If we were to define the normal function `void test(int x);` then `test(12)` would no longer be ambiguous, `12` is by default typed as `int32`
    
    void test(int x);
    void test(ImplicitExample x);
    void test(ImplicitExample2 x); // You're allowed to define this function since `ImplicitExample2` is a different type from `ImplicitExample`
    
    test(12); // OK, calls `void test(int x);`
    test(ImplicitExample(12)); // OK
    test(ImplicitExample2(12)); // OK

### Alias Types

Function overloading is based on the full path of the type specified. An example of how overloading works is as follows:
    
    alias int32_t = int32;
    
    void test(int x);
    void test(int32_t x); // calling this with the value `12` will cause an the implicit cast operator to be called for the value of `12` from type `int` to type `int32_t`
    
    test(12); // OK, calls `void test(int x);`
    