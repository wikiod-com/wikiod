---
title: "Blocks"
slug: "blocks"
draft: false
images: []
weight: 9881
type: docs
toc: true
---

## Syntax
-
    // Declare as a local variable:     

    returnType (^blockName)(parameterType1, parameterType2, ...) = ^returnType(argument1, argument2, ...) {...};
-
    // Declare as a property:    

    @property (nonatomic, copy, nullability) returnType (^blockName)(parameterTypes);

- 
    // Declare as a method parameter:

    \- (void)someMethodThatTakesABlock:(returnType (^nullability)(parameterTypes))blockName;

- 
    // Declare as an argument to a method call:

    [someObject someMethodThatTakesABlock:^returnType (parameters) {...}];

- 
    // Declare as a typedef:

    typedef returnType (^TypeName)(parameterTypes);

    TypeName blockName = ^returnType(parameters) {...};

-
    // Declare a C function return a block object:

    BLOCK_RETURN_TYPE (^function_name(function parameters))(BLOCK_PARAMETER_TYPE);

Blocks are specified by the [Language Specification for Blocks](http://clang.llvm.org/docs/BlockLanguageSpec.html) for C, Objective-C, C++ and Objective-C++.

Additionally, the Blocks ABI is defined by the [Block Implementation Specification](http://clang.llvm.org/docs/Block-ABI-Apple.html).

## Block Typedefs
    typedef double (^Operation)(double first, double second);

If you declare a block type as a typedef, you can then use the new type name instead of the full description of the arguments and return values. This defines `Operation` as a block that takes two doubles and returns a double.

The type can be used for the parameter of a method:

    - (double)doWithOperation:(Operation)operation 
                        first:(double)first 
                       second:(double)second;

or as a variable type:

    Operation addition = ^double(double first, double second){
        return first + second;
    };

    // Returns 3.0
    [self doWithOperation:addition
                    first:1.0
                   second:2.0];

Without the typedef, this is much messier:

    - (double)doWithOperation:(double (^)(double, double))operation
                        first:(double)first
                       second:(double)second;

    double (^addition)(double, double) = // ...

## Blocks as Properties
    @interface MyObject : MySuperclass
    
    @property (copy) void (^blockProperty)(NSString *string);
    
    @end

When assigning, since `self` retains `blockProperty`, block should not contain a strong reference to self.  Those mutual strong references are called a "retain cycle" and will prevent the release of either object.

    __weak __typeof(self) weakSelf = self;
    self.blockProperty = ^(NSString *string) {
        // refer only to weakSelf here.  self will cause a retain cycle
    };

It is highly unlikely, but `self` might be deallocated inside the block, somewhere during the execution. In this case `weakSelf` becomes `nil` and all messages to it have no desired effect. This might leave the app in an unknown state. This can be avoided by retaining `weakSelf` with a `__strong` ivar during block execution and clean up afterward.

    __weak __typeof(self) weakSelf = self;
    self.blockProperty = ^(NSString *string) {
        __strong __typeof(weakSelf) strongSelf = weakSelf;
        // refer only to strongSelf here.
        // ...
        // At the end of execution, clean up the reference
        strongSelf = nil;
    };


## Blocks as Method Parameters
    - (void)methodWithBlock:(returnType (^)(paramType1, paramType2, ...))name;

## Blocks as local variables
    returnType (^blockName)(parameterType1, parameterType2, ...) = ^returnType(argument1, argument2, ...) {...};    

    float (^square)(float) = ^(float x) {return x*x;};

    square(5); // resolves to 25
    square(-7); // resolves to 49

Here's an example with no return and no parameters:

    NSMutableDictionary *localStatus;
    void (^logStatus)() = ^(void){ [MYUniversalLogger logCurrentStatus:localStatus]};
    
    // Insert some code to add useful status information
    // to localStatus dictionary 
    
    logStatus(); // this will call the block with the current localStatus


## Defining and Assigning
A block that performs addition of two double precision numbers, assigned to variable `addition`:

    double (^addition)(double, double) = ^double(double first, double second){
        return first + second;
    };
    
The block can be subsequently called like so:

    double result = addition(1.0, 2.0); // result == 3.0

