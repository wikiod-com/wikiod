---
title: "NSBundle"
slug: "nsbundle"
draft: false
images: []
weight: 9979
type: docs
toc: true
---

## Getting Bundle by Path
1. Locating a Cocoa bundle using its path
>    To obtain the bundle at a specific path using Cocoa, call the ***bundleWithPath:*** class method of the **NSBundle**

>        NSBundle *myBundle;
>        // obtain a reference to a loadable bundle 
>        myBundle = [NSBundle bundleWithPath:@"/Library/MyBundle.bundle";
2. Locating a Cocoa Foundation bundle using its Path
>    To obtain the bundle at a specific path using Core Foundation, call the ***CFBundleCreate*** function and must use **CFURLRef** type.

>        CFURLRef bundleURL;
>        CFBundleRef myBundle;
>        // Make a CFURLRef from the CFString representation of the bundle's path.
>        bundleURL = CFURLCreateWithFileSystemPath(kCFAllocatorDefault, CFSTR("/Library/MyBundle.bundle"), kCFURLPOSIXPathStyle, true);
>        // Make a bundle instance using the URLRef.
>        myBundle = CFBundleCreate(kCFAllocatorDefault, bundeURL);
>        // You can release the URL now.
>        CFRelease(bundleURL);
>        // Use the bundle ...
>        // Release the bundle when done.
>        CFRelease(myBundle);

## Getting the Main Bundle
 1. Getting a reference to the main bundle using Cocoa. 
>    To get the main bundle in Cocoa application, call the ***mainBundle*** class method of the **NSBundle** class.

>        NSBundle *mainBundle;
>        // Get the main bundle for the app;
>        mainBundle = [NSBundle mainBundle];
2. Getting a reference to the main bundle using Core Foundation.
>    Use the ***CFBundleGetMainBundle*** function to retrieve the main bundle for your C-based application.

>        CFBundleRef mainBundle;
>        // Get the main bundle for the app
>        mainBundle = CFBundleGetMainBundle();
        

