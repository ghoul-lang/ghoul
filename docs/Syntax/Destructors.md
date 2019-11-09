## Destructor Handling
Destructors work mostly the same way as C++ (I say mostly since I haven't read the actual C++ standard, I'm only basing this on how I've seen C++ work)

## Early Returning
In the case that a `return` is placed early in a destructor, the destructor STILL calls the base destructor and destructs the member variables.
The best way to think of it is that a destructor calls the base destructor and destructs the member variables at all logical return points.
I.e.:
    
    struct ExampleStruct : BaseStruct
    {
        box<OtherStruct> otherStruct;
        
        ~ExampleStruct()
        {
            if (any volatile reason that cannot be optimized out)
            {
                // Destruct `otherStruct`
                // Call `BaseStruct::~BaseStruct();`
                return;
            }
            
            // Destruct `otherStruct`
            // Call `BaseStruct::~BaseStruct();`
        }
    }

This would logically be the same as
    
    struct ExampleStruct : BaseStruct
    {
        box<OtherStruct> otherStruct;
        
        ~ExampleStruct()
        {
            if (any volatile reason that cannot be optimized out)
            {
                return;
            }
            
            return;
        }
    }

Even though there is not an explicit return at the end of the destructor in the first example there is still a logical return there.
Because of this we still call everything the same way, whether the return is implicit or explicit