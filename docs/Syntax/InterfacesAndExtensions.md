## Interfaces & Extensions

### Interfaces

Interfaces in GUL have a syntax similar to a normal class/struct except that they cannot have visibility modifiers or member variables.
An interface can ONLY hold functions and properties. These functions and properties can be marked as `virtual` or `abstract` within the interface
Implementations of `interface` member functions and properties can be upgraded to `virtual` or `abstract` within the normal implementing class.
Interfaces CAN give default implementations for their methods.
Interface variables MUST be either a reference or a pointer.

Normal way to implement an `interface`

    public interface IToString
	{
	    string toString() const;
	}
	
	public class DateTime : IToString
	{
	    public string toString() const
		{
			/* You get the idea */
		}
	}
	
	// Or `virtual` & `abstract` upgraded
	public class Object : IToString
	{
		public virtual string toString() const
		{
			return "[object " + (&this).toString() + "]";
		}
	}
    
Also note the following errors:
    
	public interface IToString
	{
	    string toString() const; // OK
		virtual string toString() const; // Error - interfaces cannot declare their functions as `virtual`
		abstract string toString() const; // Error - interfaces cannot declare their functions as `abstract`
		public string toString() const; // Error - interfaces cannot declare their functions with any visibility modifier
		
		string toString() const // OK - interfaces can give default implementations
		{
			return "interface IToString not implemented!";
		}
	}
	
	// Must be reference or pointer...
	int main()
	{
		object obj = object();
		IToString objToString = (IToString)obj; // Error - cannot move variable `obj` into `objToString`
		IToString& objToString = (IToString)obj; // OK - you can take a reference to a class that implements the interface
		
		unsafe (rawptr)
		{
			IToString* ptrObjToString = (IToString*)&obj; // OK - you can take a pointer to a class that implements the interface (as long as you are in an `unsafe` block if you're using a raw pointer)
		}
	}
	
### Extensions

Extensions give the ability to extend already implemented classes and structs to give them new, non-static functions.
Extensions can also implement interfaces for an already defined class.

Examples for a normal extension without interfaces:

    extension string
	{
	    public int toInt() immut
		{
		    return int.tryParse(this);
		}
	}
	
	extension int32
	{
		public int max() immut { return 2147483647; }
		public int min() immut { return -2147483648; }
	}
	
An example for an extension with an interface:
    
	extension ulong : IToString
	{
	    public string toString() const
		{
			/* You get the idea... */
		}
	}
	
Some things to not about extensions are that they DO NOT modify the already implemented types. 
Extensions CANNOT define virtual/abstract members, extensions CANNOT define new member variables.
Extensions methods can ONLY be accessed by non-compiled code that is within the scope of the extension -
EXCEPT in the case of templated functions that access the extended type OR when the extended type is cast to an extended interface.

Examples of uses:
    
    extension ulong : IToString
	{
	    public abstract string toString() const; // Error - extensions CANNOT define virtual or abstract members
		
		public string toString() const
		{
			return string(this);
		}
	}
	
	// Compile-time reflection
	int main(string[] args)
	{
		ulong test = 0xdeadbeef;
		// `writeline` is defined as `writeline<typename... Ts>(string format, Ts... args)` 
		// which then checks if any of the `Ts` have a member function named `toString` that returns a `string`
		// Because `writeline` is a variadic template function that means it will have to be uniquely created for the current binary
		// Since it is being implemented for the current binary it then knows at compile-time that `ulong` implements `IToString` now.
		console.writeline("{0}", test); 
	}
	
	// Interface casting
	int main(string[] args)
	{
		ulong test = 0xdeadbeef;
		// Explicitly casting to an extension interface is allowed.
		IToString& toStringInterface = (IToString)test;
		
	}
