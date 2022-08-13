---
title: "Modern Objective-C"
slug: "modern-objective-c"
draft: false
images: []
weight: 9987
type: docs
toc: true
---

## Literals
Modern Objective C provides ways to reduce amount of code you need to initialize some common types. This new way is very similar to how NSString objects are initialized with constant strings.

NSNumber
--------

Old way:

    NSNumber *number = [NSNumber numberWithInt:25];

Modern way:

    NSNumber *number = @25;

Note: you can also store `BOOL` values in `NSNumber` objects using `@YES`, `@NO` or `@(someBoolValue)`;

NSArray
-------
Old way:

    NSArray *array = [[NSArray alloc] initWithObjects:@"One", @"Two", [NSNumber numberWithInt:3], @"Four", nil]; 

Modern way:

    NSArray *array = @[@"One", @"Two", @3, @"Four"];


NSDictionary
------------
Old way:

    NSDictionary *dictionary = [NSDictionary dictionaryWithObjectsAndKeys: array, @"Object", [NSNumber numberWithFloat:1.5], @"Value", @"ObjectiveC", @"Language", nil];

Modern way:

    NSDictionary *dictionary = @{@"Object": array, @"Value": @1.5, @"Language": @"ObjectiveC"};


## Container subscripting
In modern Objective C syntax you can get values from `NSArray` and `NSDictionary` containers using container subscripting.

Old way:

    NSObject *object1 = [array objectAtIndex:1];
    NSObject *object2 = [dictionary objectForKey:@"Value"];

Modern way:

    NSObject *object1 = array[1];
    NSObject *object2 = dictionary[@"Value"];

----------

You can also insert objects into arrays and set objects for keys in dictionaries in a cleaner way:

Old way:

    // replacing at specific index
    [mutableArray replaceObjectAtIndex:1 withObject:@"NewValue"];
    // adding a new value to the end
    [mutableArray addObject:@"NewValue"];
    
    [mutableDictionary setObject:@"NewValue" forKey:@"NewKey"];

Modern way:

    mutableArray[1] = @"NewValue";
    mutableArray[[mutableArray count]] = @"NewValue";

    mutableDictionary[@"NewKey"] = @"NewValue";


