# GULC

GUL (pronounced "Ghoul") is a WIP language meant to be an improvement upon C++ and C# with a focus on making a language that can handle systems programming, OS development, and GUI development while still being easy to use.

# Current Support

| Feature              | Status      | Basic Example                                             |
| -------------------- | ----------- | --------------------------------------------------------- |
| Functions            | Working     | `int main() { }`                                          |
| Namespaces           | Working     | `namespace std.io {}`                                     |
| Global Variables     | Working     | `float PI = 3.14;`                                        |
| Template Functions   | Working     | `T Example<T>(T arg) {}`                                  |
| Structs              | Working     | `struct Example : BaseStruct {  }`                        |
| Move Constructors    | Working     | `[Move] public Example(Example& other) { }`               |
| Copy Constructors    | Working     | `[Copy] public Example(Example const& other) { }`         |
| Template Stucts      | Planned     | `struct Box<T> { public T* pointer; }`                    |
| Operator Overloading | In Progress | `public Example operator infix+(Example& rightSide) {}`   |
| Custom Operators     | In Progress | `public void operator prefix delete(Example* pointer) {}` |
| Custom Operator Call | In Progress | `delete! pointer; // requires ending '!'`                 |

# Planned Syntax Examples

Template Struct, template function, operator overloading:
    
    namespace std
    {
        public struct Box<T>
        {
            private T* pointer;
            
            public Box(T* pointer)
                // Process will abort if `pointer` is null. Compiler will error if it can detect null at compile time
                requires pointer != null
            {
                this->pointer = pointer;
            }
            
            public T& operator prefix *()
            {
                return *pointer;
            }
            
            // Same as C++ for how suffix `const` works
            public T const& operator prefix *() const
            {
                return *pointer;
            }
            
            ~Box
            {
                delete! pointer;
            }
        }
    }
    
Custom Operators:
    
    // There won't actually be an `append` operator, just meant to be an example
    namespace std.collections
    {
        public struct List<T>
        {
            public void operator infix append(T& value)
            {
                // Call the existing `List<T>::Append` function.
                this.Append(value);
            }
        }
    }
    
    int main(string[] args)
    {
        List<string> argsList;
        
        // `foreach!` is a standard library created macro, macro syntax not yet finalized.
        foreach! (string arg in args)
        {
            // Call the new `append` custom operator, requires the custom infix operator to end with `~`
            argsList append~ arg;
            // NOTE: The below syntax is illegal, the `~` CANNOT have a space before it as this can lead to confusion.
            //argsList append ~arg;
        }
    }
    
Contracts:

Constracts will handle both setting template type requirements and any normal contracts.
    
    public struct List<T>
        // NOTE: Same as C#'s `where` clause
        requires T : SomeType
    {
        public T& Get(usize index)
            // Process will be abandoned if `index` is greater than or equal to the length of the List
            requires index < this.Length
        {
            return internalPointer[index];
        }
        
        public void Clear()
            // This will help notify the compiler that the length of the List will be 0 after calling `Clear`
            // NOTE: In an optimizing compiler we can know that `Get` cannot be called immediately after `Clear`
            //       due to the requirements for `Get` failing at compile time
            ensures Length == 0
        {
            delete! internalPointer;
            // NOTE: In an optimizing compiler we can elide the `ensures Length == 0` check since we can verifiable know
            //       `Length == 0` at the end of `Clear`
            Length = 0;
        }
    }
    
Exceptions:
    
    public extension int
    {
        // Functions that throw exceptions MUST be marked throws
        public static int Parse(string const& s) throws
        {
            throw Exception("error message");
        }
    }
    
    namespace std.io
    {
        public struct File
        {
            // NOTE: You can optionally say what the exception that is thrown is, this is not required though.
            public File Open(string const& filePath) 
                // NOTE: Marking a function as `throws` requires you to either catch or pass ALL exceptions
                //       if you mark only `throws` with no type you MUST `catch {}` with a blank,
                //       if you mark with `throws ExceptionType` you MUST `catch (ExceptionType et) {}`
                throws FileNotFoundException,
                       InvalidFilePermissionsException
            {
                // ...
            }
        }
    }
    
    int Main(string[] args)
    {
        // You don't have to be marked `throws` if you catch ALL exceptions
        try
        {
            return int.Parse(args[1]);
        }
        catch
        {
            std.io.Console.WriteLine("Could not parse input!");
            return 1;
        }
    }
    
