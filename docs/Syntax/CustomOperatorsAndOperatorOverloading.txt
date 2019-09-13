## Custom Operators And Overloading Operators

### Overloading
#### Overload [] and [,,,] Variants

You can overload the [] and [,] variants in GUL similar to how you can in C++ and C#.
    
	int operator [int x, int y]
	{
		return this.internalArray[x, y];
	}
	
	TValue operator [TKey dictLookup]
	{
		return this.internalDict[dictLookup];
	}
	
#### Overload ()

You can overload the () and variants in GUL similar to how you can in C++.
    
    T operator ()()
	{
	    return /* You get the idea. */;
	}
	
	int operator ()(int param1)
	{
		return param1;
	}
	
#### Overload . and ->

Unlike C++ and C#, you are allowed to override the operator `.` and `->` IF AND ONLY IF the class/struct it is implemented in has NO public facing functions that can be accessed.
    
	// This example is how we might use `.` for the implementation of `box<T>`
	public T&mut operator .()
	{
		return *this.rawPtr;
	}
	
	// We might also limit this so it is only show for `box<T*>` variants in this example...
	public T*&mut operator ->()
	{
		return *this.rawPtr;
	}
	
	public int getType() // Error - cannot declare public facing functions in a struct/class that defines the `.` or `->` operators
	

#### Overload Binary Operators +, -, *, /, &, |, &&, ||, +=, -=, etc.

Binary operator overloading is almost identical to how it is implemented in C++ and C#.
    
    public int128_t operator +(int128_t rhs) const { }
	
	public bool operator >=(int128_t rhs) const {  }
	
	public int128_t operator +=(int64 rhs) { }
	
IMPORTANT: If the binary operator has a `this` parameter that is marked `&mut` then it is treated as an assignment operator and should be treated the same as the normal `=` in optimizations.

#### Overload Prefix and Postfix Operators +, -, ++, --, &, *

Prefix and postfix operator overloading is nearly identical to how it is implemented in C++.
    
	public 
