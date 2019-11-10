## Visibility in GUL
GUL supports all visibility modifiers that C# has (`public`, `private`, `protected`, `internal`, `protected internal`).
They work exactly the same that they do within C# and the modifiers that also exist in C++ should work how they do in C++ too.

### Default Visibility
#### Namespace Member Visibility
To be more similar to C++, all members within a namespace are `public` by default. 
To make them `internal`, like they are by default in C#, you have to specify `internal` on the declaration.
The only legal visibility modifiers within a namespace are `public` and `internal`

#### Struct Member Visibility
GUL again copies C++ in making all members of a `struct` public by default if no visibility modifier is specified.

#### Class Member Visibility
Similar to above, GUL copies C++ in making all members of `class` private by default.

#### Interface/Trait Visibility
Members of an interface/trait are `public` and can ONLY be `public`, you CANNOT explicitly set the visibility of these members.
