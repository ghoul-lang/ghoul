## GUL vs C Type Syntax

GUL: `int*[] intArray` -> `array of pointers to int`
C: `int *intArray[];` -> `array of pointers to int`

GUL: `int[]* ptrIntArray;` -> `pointer to an array of ints`
C: `int (*ptrIntArray)[];` -> `pointer to an array of ints`, this syntax is invalid in C

GUL also supports qualifier constructors to simplify qualifying a type but still supports C type qualifiers

GUL: `int *const i;` -> `const pointer to int`
C: `int *const i;` -> `const pointer to int`

GUL: `int const* i;` -> `pointer to const int`
C: `int const* i;` -> `pointer to const int`

GUL: `const int* i;` -> `pointer to const int`
C: `const int* i;` -> `pointer to const int`

GUL: `const(int*) i;` -> `const pointer to int`
GUL: `const(int)* i;` -> `pointer to const int`

GUL also supports references, mutable (mut), and immutable (immut) types.

References are marked by the `&` operator, same as C++. That are const by default. Use `&mut` to make them writeable.
`const` is the same as C++. Within a `const` function you cannot change any data unless the variable is marked as mutable (mut).
`mut` is only used to make references writeable and to make a member of a class writeable in const functions (the only major use for this being members with the type `std.threads.mutex` or relatives as you would want to be able to lock within const function of a concurrent class)
`immut` is completely readonly. You cannot write to even `mut` variables (so you can't lock). `immut` is also transitive. `immut(int**)` is `an immutable pointer to an immutable pointer to an immutable int`

`mut int&` -> `reference to mutable int` this is unneeded. Should spit out a warning saying "`mut int` is redundant, did you mean `int &mut` or `mut(int&)` for `mutable reference to int`?"
`int mut&` -> `reference to mutable int` see above
`int &mut` -> `mutable reference to int` what the programmer most likely means when they type the above two lines.
`mut(int&)` -> `mutable reference to int` what the programmer almost 100% means when they type `mut int&`

`immut int*` -> `pointer to immutable int`
`int immut*` -> `pointer to immutable int`
`int *immut` -> `immutable pointer to immutable int`
`immut(int*)` -> `immutable pointer to immutable int`
`immut(int***)` -> `immutable pointer to immutable pointer to immutable pointer to immutable int`
`const(int***)` -> `const pointer to pointer to pointer to int`
`mut(const(int**)&)` -> `mutable reference to const pointer to pointer to int`

Type syntax is like this since GUL is meant to be an upgrade to C and C++. We steal the `const(int*)` idea from D since it is a logical conclusion that if `const int*` can work as `pointer to const int` then `const(int*)` should be allowed for `const pointer to int`

#### Function Pointers
`int fn(int, int)*` -> `pointer to function returning int with two parameters, both int`
`int* fn(int, int)*` -> `pointer to function returning a pointer to an int with two parameters, both int`
