## Member Functions

Within class/struct/etc. methods, the first variable of the function will be `T&mut this` for a non-qualified function. 
The reference is a mutable reference since the default behaviour of a function is to be a writer, non-writer functions should be qualified as `const` or `immut`.
The `this` parameter is a reference to the class rather than a pointer as `this` should NEVER be null.
Situations where the `this` variable is null is undefined behaviour.

Qualifying a member function with a `const` will change the `this` parameter to `T&const this`.
A member function that is marked as `const` is allowed to be optimized in a way that multiple calls in certain situations can be optimized to one call. (i.e. `if (funcCall() == test1 || funcCall() == test2)` can be changed to `auto funcCallResult = funcCall(); if (funcCallResult == test1 || funcCallResult == test2)`
A member function that is marked as `const` still has special permission to write to `mut` variables. The sole purpose of `mut` variables is to still allow `const` functions to lock while still gaining the optimizations from the above scenario.

Qualifying a member function with a `immut` will change the `this` parameter to `T&immut this`.
A member function that is marked as `immut` is NOT allowed to write to ANY member of `this`.
