---
title: "Block"
slug: "block"
draft: false
images: []
weight: 9968
type: docs
toc: true
---

## Syntax
- As a variable:

    returnType (^blockName)(parameterTypes) = ^returnType(parameters) {...};

- As a property:
   
    @property (nonatomic, copy) returnType (^blockName)(parameterTypes);

- As a method parameter: 
 
    \- (void)methodWithBlock:(returnType (^)(parameterTypes))blockName;
- As a typedef:

    typedef returnType (^TypeName)(parameterTypes);
    
    TypeName blockName = ^returnType(parameters) {...};
    

## Custom completion block for Custom Methods
**1- Define Your own custom Block** 

    typedef void(^myCustomCompletion)(BOOL);

**2- Create custom method which takes your custom completion block as a parameter.**

    -(void) customMethodName:(myCustomCompletion) compblock{
        //do stuff
        // check if completion block exist; if we do not check it will throw an exception
        if(complblock)
           compblock(YES);
      }
3- How to use block in your Method 

    [self customMethodName:^(BOOL finished) {
    if(finished){
        NSLog(@"success");
    }
    }];



## UIView Animations
    [UIView animateWithDuration:1.0
        animations:^{
            someView.alpha = 0;
            otherView.alpha = 1;
        }
        completion:^(BOOL finished) {
        [someView removeFromSuperview];
    }];
    

The carat “^” character defines a block. For example, `^{ … }` is a block. More specifically, it is a block that returns “void” and accepts no arguments. It is equivalent to a method such like: “- (void)something;” but there is no inherent name associated with the code block.

Define a block that can accept arguments work very similarly. To supply an argument to a block, you define the block like so: **^(BOOL someArg, NSString* someStr) { … }**. When you use API calls that support blocks, you’ll be writing blocks that look similar to this, especially for animation blocks or NSURLConnection blocks as shown in the above example.

## Modify captured variable
Block will capture variables that appeared in the same lexical scope. Normally these variables are captured as "const" value:
    
    int val = 10;
    void (^blk)(void) = ^{
        val = 20; // Error! val is a constant value and cannot be modified!
    };
    
In order to modify the variable, you need to use the __block storage type modifier.
    
    __block int val = 10;
    void (^blk)(void) = ^{
        val = 20; // Correct! val now can be modified as an ordinary variable.
    };


