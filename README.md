# GULC

GUL (pronounced "Ghoul") is a WIP language meant to be an improvement upon C++ and C# with a focus on making a language that can handle systems programming, OS development, and GUI development while still being easy to use.

GUL borrows a lot of ideas from other languages. The current planned stand out features are (where any idea is borrowed from will be in parenthesis):
 
 * Template support (C++) but with C# syntax (`public class Example<int i> {}` instead of `template<int i> class Example {}`)
 * Lifetime support (Rust) (`public class Example<lifetime a> {}`, maybe implemented using special attributes - `[lifetimes<a, b, c>] public class Example { [lifetime<a>] public int& intRef; }`)
 * Structs/classes destruct when scope is lost (C++)
 * Glass UI (inspired by old .NET project I worked on)
 * Interfaces can be added to any already defined type (interfaces are similar to Rust traits) (Rust?)
 * Traits entirely instead of interfaces (Rust) (since `trait` in GUL will support everything C# `interface` handles while also supporting default implementations of functions and being able to be implemented on already defined traits)
 * Explicitly defined `generic` functions (C#-ish) (since C++ template functions cannot be virtual/abstract there will also be `generic` functions that will allow C#-style generic functions to be declared `virtual` with the potential runtime penalties that come from them)
 * Template struct overloading (`public struct Example<T>{}` and `public struct Example<T, G>{}` are two, entirely different types)
 * TODO: Add the rest of the features...
