## Temporary Objects Lifetimes

In GUL you have the ability to create temporary objects and create references to those temporary object, similar to 
C++ and Rust. The lifetime of a temporary object ends at the end of a full expression EXCEPT for cases where a 
pointer or reference points to the temporary object. Basically, if an temporary object lvalue is not converted to an 
rvalue then it will survive until the full statement (e.g. CompoundStmt, ForStmt, etc.) is finished.

### Examples
    
    ExampleStruct getExampleStruct() { /* ... */ }
    int getExampleStructHash(ExampleStruct ex);
    
    ExampleLabeledStmt: {
        ExampleStruct t = getExampleStruct(); // A temporary object is created and then moved into `t`, logically the destructor is called for the temporary but in implementation this would be optimized out... 
        ExampleStruct const& refEx = getExampleStruct(); // A temporary object is created and then a reference to it is made
        ExampleStruct mut& refEx = getExampleStruct(); // Illegal, result of `getExampleStruct` is an rvalue
        int hash = getExampleStructHash(getExampleStruct()); // The result of `getExampleStruct` is logically destructed after the last ';' of the expression
        
        getExampleStruct(); // The result of `getExampleStruct()` is immediately destructed
        
        // The result of `getExampleStruct` is destructed before the `++i` NOT after finishing looping...
        for (int i = 0; i < getExampleStruct().length; ++i)
        {
            
        }
        
        // This assumes `ExampleStruct` is enumerable...
        foreach! (int& val : getExampleStruct()) // `getExampleStruct` is referenced here.
        {
            
        } // the result of `getExampleStruct` is destructed here...
    }
