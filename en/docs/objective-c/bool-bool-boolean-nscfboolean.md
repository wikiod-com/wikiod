---
title: "BOOL  bool  Boolean  NSCFBoolean"
slug: "bool--bool--boolean--nscfboolean"
draft: false
images: []
weight: 9985
type: docs
toc: true
---

## BOOL/Boolean/bool/NSCFBoolean
 1. bool is a datatype defined in C99. 
 2. Boolean values are used in conditionals, such as if or while
        statements, to conditionally perform logic or repeat execution. When
        evaluating a conditional statement, the value 0 is considered
        “false”, while any other value is considered “true”. Because NULL
        and nil are defined as 0, conditional statements on these
        nonexistent values are also evaluated as “false”.
 3. BOOL is an Objective-C type defined as signed char with the macros
        YES and NO to represent true and false

From the definition in objc.h:

    #if (TARGET_OS_IPHONE && __LP64__)  ||  TARGET_OS_WATCH
    typedef bool BOOL;
    #else
    typedef signed char BOOL; 
    // BOOL is explicitly signed so @encode(BOOL) == "c" rather than "C" 
    // even if -funsigned-char is used.
    #endif
    
    #define YES ((BOOL)1)
    #define NO  ((BOOL)0)

 4. NSCFBoolean is a private class in the NSNumber class cluster. It is
    a bridge to the CFBooleanRef type, which is used to wrap boolean
    values for Core Foundation property lists and collections. CFBoolean
    defines the constants kCFBooleanTrue and kCFBooleanFalse. Because
    CFNumberRef and CFBooleanRef are different types in Core Foundation,
    it makes sense that they are represented by different bridging
    classes in NSNumber.



## BOOL VS Boolean
**BOOL**

 - Apple's Objective-C frameworks and most Objective-C/Cocoa code uses  
   BOOL.
 - Use BOOL in objective-C, when dealing with any CoreFoundation    APIs

**Boolean**

 - Boolean is an old Carbon keyword , defined as an unsigned char

