# GULC

GUL (pronounced "Ghoul") is a WIP language meant to be an improvement upon C++ and C# with a focus on making a language that can handle systems programming, OS development, and GUI development while still being easy to use.

GUL borrows a lot of ideas from other languages. The current planned stand out features are (where any idea is borrowed from will be in parenthesis):
 
 * Template support (C++) but with C# syntax (`public class Example<int i> {}` instead of `template<int i> class Example {}`)
 * Lifetime support (Rust) (`public class Example<lifetime a> {}`)
 * Structs/classes destruct when scope is lost (C++)
 * Structs/classes are move-only by default, must be explicitly marked `copyable` for copying to work (this is to prevent large, unneeded copies that can slow down a program and encourages the user of references)
 * Glass UI (inspired by old .NET project I worked on)
 * Interfaces can be added to any already defined type (interfaces are similar to Rust traits) (Rust?)
 * TODO: Add the rest of the features...

License is not set in stone at the moment, it might change at any moment.
