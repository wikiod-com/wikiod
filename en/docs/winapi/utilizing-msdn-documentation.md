---
title: "Utilizing MSDN Documentation"
slug: "utilizing-msdn-documentation"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

The Windows API is vast, and contains a lot of features. The size of the API is such that no one can know all of it. While there are many resources like StackOverflow, there is no substitute for the official documentation. 

**Examples of Documentation:**
- **Topic Overview:** [Desktop Window Manager Performance Considerations and Best Practices](https://msdn.microsoft.com/en-us/library/windows/desktop/aa969536.aspx)
- **Samples:** [Customize an Iconic Thumbnail and a Live Preview Bitmap](https://msdn.microsoft.com/en-us/library/windows/desktop/ff819048.aspx)
- **Functions:** [DwmSetIconicThumbnail function](https://msdn.microsoft.com/en-us/library/windows/desktop/dd389411.aspx)

## Types of Documentation Available
The MSDN library contains several different types of documentation which can be used for implementing features.

- **Topic Overviews** These are broad overviews of topics intended to provide a general understanding of an API. These overviews also often outline best practices, and implementation strategies.
- **Samples** Demonstrate the use of particular APIs. These are generally highly simplified, don't necessarily do error checking, and typically don't use frameworks like MFC or ATL. They provide a starting point for using features.
- **Reference** Details all of the elements of each API. This includes constants/enumerations, interfaces, functions and classes.

_Note: Many Microsoft employees also maintain blogs, like Raymond Chen's [The Old New Thing](https://blogs.msdn.microsoft.com/oldnewthing/) that can supplement the documentation, but these blogs are not a substitute for the documentation._

## Finding Documentation for a Feature
Finding documentation for a feature is often as simple as a search using a good search engine. If that fails, or if unsure about specific terms, the [Windows API Index](https://msdn.microsoft.com/en-us/library/windows/desktop/ff818516.aspx) can help locate specific features. Documentation for methods, interfaces, enumerations and constants can usually be found by searching for the name using a search engine. Additionally, the [Windows Dev Center](https://developer.microsoft.com/en-us/windows) can provide a valuable starting point.

## Using Function Documentation
The documentation for a function is broken down into several sections:

Overview
===

Describes what the function is used for. This section will also show information about whether the function is depreciated, or may be unavailable in future versions.

Syntax
===

Shows the declaration of the function from the appropriate source header. It is a quick reference to the function's signature.

Parameters
===

Explains each of the parameters, whether the parameter is input or output, and other important considerations.

Return Value
===

This section explains the result of the function call, including how to detect errors, and what additional information is available. (For example, this section will state explicitly if `GetLastError` will provide additional error handling information.)

Remarks
===
Covers any additional information required to use the function, such as, information about supporting functions, obtaining appropriate handles, and disposal of resources.

Examples
===
If this section is available, it has an example of the appropriate use of the function to use as a starting point for implementation.

Requirements
===
Gives important information about prerequisites for calling the function. This information includes:

- **Minimum Supported Client/Server** First version of the operating system (supported by Microsoft) to provide the function.  
 <sub>(Note that this field is notoriously misleading. Often, functions are supported in an earlier version of the operating system, but this field only shows the earliest version that is *currently supported by Microsoft*. For example, the `CreateWindow` function has been supported since Windows 1.0, but the documentation only shows that it has been supported since Windows 2000. The online version of the MSDN documentation does not indicate that *any* function was supported in a version of Windows prior to 2000, even though many were. For legacy development, you will need to consult an older version of the SDK documentation, such as might have been shipped on an MSDN CD-ROM. Or, just look in the header files.)</sub>
- **Header** The SDK header to `#include` that contains the function declaration. If the function isn't available in a header, this will show information about the procedure to call the function (usually calling `GetProcAddress` to do run-time dynamic linking).
- **Library** The library file to pass to the linker to resolve the exported functions.
- **DLL** The file (as shipped with the operating system) that contains the exported function.
- **End of Client/Server Support** The last version of Windows to officially support the API.
- **Unicode and ANSI names** For string functions that have both Unicode and ANSI variants, this lists the actual exported names for the two functions. This is usually just the function name with a `W` or `A` suffix (respectively).




