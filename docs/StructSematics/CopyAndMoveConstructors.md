# Copy & Move Constructors
In GUL you define copy and move constructors using the `[Copy]` and `[Move]` attributes on a constructor.

## Copy Constructor
To declare a copy constructor you do the following:
    
    struct Example
    {
        [Copy]
        public Example(Example const& other)
        {
            // ...
        }
    }
    
Copy constructors take a const reference as its only argument. You CANNOT modify the `other` struct variable in a copy.

If a copy constructor is not explicitly provided, a default copy constructor will be created for you.

## Move Constructor
To declare a move constructor you do basically the same thing:
    
    struct Example
    {
        [Move]
        public Example(Example& other)
        {
            // ...
        }
    }
    
Move constructors take a mutable reference as its only argument. You CAN modify the `other` struct variable in a move.

If a move constructor is not explicitly provided, a default move constructor will be created for you.

## Deleted Copy & Move Constructors
To delete the copy and move constructors, you do the following:
    
    [Move(false)] // Delete the move constructor
    [Copy(false)] // Delete the copy constructor
    struct Example
    {
        // Struct is now not copyable or movable
    }
    
If struct `A` has a deleted move constructor and struct `B` inherits from struct `A` without explicitly deleting their own move constructor then that is an error.

Example:
    
    [Move(false)] // Delete the move constructor
    struct A
    {
        
    }
    
    struct B : A
    {
        // Error: Base class `A` has a deleted move constructor, struct `B` MUST have attribute `[Move(false)]` to delete its own move constructor!
    }
    
The same above also applies to the copy constructor.