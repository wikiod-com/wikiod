---
title: "NSJSONSerialization"
slug: "nsjsonserialization"
draft: false
images: []
weight: 9961
type: docs
toc: true
---

## Syntax



   + (id)JSONObjectWithData:(NSData *)data
                 options:(NSJSONReadingOptions)opt
                   error:(NSError * _Nullable *)error

## Parameters

| Operator|Description|
| ------ | ------ |
| data   | A data object containing JSON data   |
| opt |Options for reading the JSON data and creating the Foundation objects. |
| error | If an error occurs, upon return contains an NSError object that describes the problem. |




NSJSONSerialization is Available in iOS 5.0 and later An object that may be converted to JSON must have the following properties:

- The top level object is an NSArray or NSDictionary.

- All objects are instances of NSString, NSNumber, NSArray, NSDictionary, or NSNull.

- All dictionary keys are instances of NSString.

- Numbers are not NaN or infinity.

## JSON Parsing using NSJSONSerialization Objective c

    NSError *e = nil;
    NSString *jsonString = @"[{\"id\": \"1\", \"name\":\"sam\"}]";
    NSData *data = [jsonString dataUsingEncoding:NSUTF8StringEncoding];

    NSArray *jsonArray = [NSJSONSerialization JSONObjectWithData: data options:  NSJSONReadingMutableContainers error: &e];

    if (!jsonArray) {
        NSLog(@"Error parsing JSON: %@", e);
    } else {
        for(NSDictionary *item in jsonArray) {
            NSLog(@"Item: %@", item);
        }
    }

**Output:**

    Item: {
        id = 1;
        name = sam;
    }

**Example 2:Using contents of url:**

    //Parsing:

    NSData *data = [NSData dataWithContentsOfURL:@“URL HERE”];
    NSError *error;
    NSDictionary *json = [NSJSONSerialization JSONObjectWithData:data options:kNilOptions error:&error];
    NSLog(@“json :%@”,json);


Sample response:

    json: {
        MESSAGE = “Test Message";
        RESPONSE =(
                    {
                email = "test@gmail.com";
                id = 15;
                phone = 1234567890;
                name = Staffy;
            }
        );
        STATUS = SUCCESS;
    }


     NSMutableDictionary *response = [[[json valueForKey:@"RESPONSE"] objectAtIndex:0]mutableCopy];
     NSString *nameStr = [response valueForKey:@"name"];
     NSString *emailIdStr = [response valueForKey:@"email"];




