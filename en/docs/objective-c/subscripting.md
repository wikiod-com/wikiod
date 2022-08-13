---
title: "Subscripting"
slug: "subscripting"
draft: false
images: []
weight: 9975
type: docs
toc: true
---

## Subscripts with NSArray
Subscripts can be used to simplify retrieving and setting elements in an array. Given the following array

    NSArray *fruit = @[@"Apples", @"Bananas", @"Cherries"];

This line

    [fruit objectAtIndex: 1];

Can be replaced by

    fruit[1];

They can also be used to set an element in a mutable array.

    NSMutableArray *fruit = [@[@"Apples", @"Bananas", @"Cherries"] mutableCopy];
    fruit[1] = @"Blueberries";
    NSLog(@"%@", fruit[1]); //Blueberries

If the index of the subscript equals the count of the array, the element will be appended to the array.

Repeated subscripts may be used to access elements of nested arrays.

    NSArray *fruit = @[@"Apples", @"Bananas", @"Cherries"];
    NSArray *vegetables = @[@"Avocado", @"Beans", @"Carrots"];
    NSArray *produce = @[fruit, vegetables];
        
    NSLog(@"%@", produce[0][1]); //Bananas



## Custom Subscripting
You can add subscripting to your own classes by implementing the required methods.

For indexed subscripting (like arrays):

    - (id)objectAtIndexedSubscript:(NSUInteger)idx
    - (void)setObject:(id)obj atIndexedSubscript:(NSUInteger)idx

For keyed subscripting (like dictionaries):

    - (id)objectForKeyedSubscript:(id)key
    - (void)setObject:(id)obj forKeyedSubscript:(id <NSCopying>)key

## Subscripts with NSDictionary
Subscripts can also be used with NSDictionary and NSMutableDictionary. The following code:

    NSMutableDictionary *myDictionary = [@{@"Foo": @"Bar"} mutableCopy];
    [myDictionary setObject:@"Baz" forKey:@"Foo"];
    NSLog(@"%@", [myDictionary objectForKey:@"Foo"]); // Baz

Can be shortened to:

    NSMutableDictionary *myDictionary = [@{@"Foo": @"Bar"} mutableCopy];
    myDictionary[@"Foo"] = @"Baz";
    NSLog(@"%@", myDictionary[@"Foo"]); // Baz

