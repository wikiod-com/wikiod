---
title: "plist iOS"
slug: "plist-ios"
draft: false
images: []
weight: 9977
type: docs
toc: true
---

  Plist is used for storage of data in iOS app. Plist save data in
    form of Array and Dictionaries. In plist we can save data as:
    
     1. Static data to be used in app.
     2. Data that will be coming from server.



## Example:
**1. Static data to be used in app.**

To save static data in plist follow these methods:

 **a) Add a new file** 
 
[![enter image description here][1]][1]

 **b) Click Property list in Resources** 

[![enter image description here][2]][2]


**c) Name the propertylist and a file will be created as(data.plist here)**

[![enter image description here][3]][3]


d) You can create a plist of Arrays and Dictionaries as:

[![enter image description here][4]][4]


  [1]: https://i.stack.imgur.com/mGHnw.png
  [2]: https://i.stack.imgur.com/XlN4t.png
  [3]: https://i.stack.imgur.com/wr0El.png
  [4]: https://i.stack.imgur.com/pfxfW.png


// Read plist from bundle and get Root Dictionary out of it

    NSDictionary *dictRoot = [NSDictionary dictionaryWithContentsOfFile:[[NSBundle mainBundle] pathForResource:@"Data" ofType:@"plist"]];

// Your dictionary contains an array of dictionary
// Now pull an Array out of it.

    NSArray *arrayList = [NSArray arrayWithArray:[dictRoot objectForKey:@"Object1"]];

    for(int i=0; i< [arrayList count]; i++)
    {
        NSMutableDictionary *details=[arrayList objectAtIndex:i];
    }




## Save and edit/delete data from Plist
You have already created a plist. This plist will remain same in app. If you want to edit the data in this plist, add new data in plist or remove data from plist, you can't make changes in this file. 

For this purpose you will have to store your plist in Document Directory. You can edit your plist saved in document directory. 

**Save plist in document directory as:**

    NSString *filePath = [[NSBundle mainBundle] pathForResource:@"Data" ofType:@"plist"];

    NSDictionary *dict = [[NSDictionary alloc] initWithContentsOfFile:filePath];

    NSDictionary *plistDict = dict;
    
    NSFileManager *fileManager = [NSFileManager defaultManager];

    NSString *error = nil;
    
    NSData *plistData = [NSPropertyListSerialization dataFromPropertyList:plistDict format:NSPropertyListXMLFormat_v1_0 errorDescription:&error];
    
    if (![fileManager fileExistsAtPath: plistPath]) {
        
        if(plistData)
        {
            [plistData writeToFile:plistPath atomically:YES];
        }
    }
    else
    {
         
    }

**Retreive data from Plist as:**

        NSArray *paths = NSSearchPathForDirectoriesInDomains (NSDocumentDirectory, NSUserDomainMask, YES);
        NSString *documentsPath = [paths objectAtIndex:0];
        NSString *plistPath = [documentsPath stringByAppendingPathComponent:@"Data.plist"];
        NSDictionary *dict = [[NSDictionary alloc] initWithContentsOfFile:plistPath];
        
        NSArray *usersArray = [dict objectForKey:@"Object1"];


You can edit remove, add new data as per your requirement and save the plist again to Document Directory.






