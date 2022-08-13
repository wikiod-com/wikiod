---
title: "Cross-Platform Development"
slug: "cross-platform-development"
draft: false
images: []
weight: 9985
type: docs
toc: true
---

## TargetConditionals
The system header `TargetConditionals.h` defines several macros which you can use from C and Objective-C to determine which platform you're using.

    #import <TargetConditionals.h>  // imported automatically with Foundation

    - (void)doSomethingPlatformSpecific {
    #if TARGET_OS_IOS
        // code that is compiled for iPhone / iPhone Simulator
    #elif TARGET_OS_MAC && !TARGET_OS_IPHONE
        // code that is compiled for OS X only
    #else
        // code that is compiled for other platforms
    #endif
    }

The values of the macros are:

<!-- if version [gte 7.0] -->
When using the iOS 9.1, tvOS 9.0, watchOS 2.0, OS X 10.11 or newer SDKs:

|                   Macro | Mac | iOS | iOS simulator | Watch | Watch simulator | TV  | TV simulator |
|-----------------------: | :-: | :-: | :-----: | :---: | :-------: | :-: | :----: |
|`TARGET_OS_MAC`          |**1**|**1**|  **1**  | **1** |   **1**   |**1**|  **1** |
|`TARGET_OS_IPHONE`       |  0  |**1**|  **1**  | **1** |   **1**   |**1**|  **1** |
|`TARGET_OS_IOS`          |  0  |**1**|  **1**  |   0   |     0     |  0  |    0   |
|`TARGET_OS_WATCH`        |  0  |  0  |    0    | **1** |   **1**   |  0  |    0   |
|`TARGET_OS_TV`           |  0  |  0  |    0    |   0   |     0     |**1**|  **1** |
|`TARGET_OS_SIMULATOR`    |  0  |  0  |  **1**  |   0   |   **1**   |  0  |  **1** |
|`TARGET_OS_EMBEDDED`     |  0  |**1**|    0    | **1** |     0     |**1**|    0   |
|`TARGET_IPHONE_SIMULATOR`|  0  |  0  |  **1**  |   0   |   **1**   |  0  |  **1** |
<!-- end version if -->

<!-- if version [lt 7.0] -->
When using the iOS 8.4, OS X 10.10, or older SDKs:

|                    Macro | Mac | iOS | iOS simulator |
|------------------------: | :-: | :-: | :-----------: |
|          `TARGET_OS_MAC` |**1**|**1**|     **1**     |
|       `TARGET_OS_IPHONE` |  0  |**1**|     **1**     |
|     `TARGET_OS_EMBEDDED` |  0  |**1**|       0       |
|`TARGET_IPHONE_SIMULATOR` |  0  |  0  |     **1**     |
<!-- end version if -->

