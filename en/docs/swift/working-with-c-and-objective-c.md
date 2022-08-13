---
title: "Working with C and Objective-C"
slug: "working-with-c-and-objective-c"
draft: false
images: []
weight: 9883
type: docs
toc: true
---

For further information, see Apple's documentation on [Using Swift with Cocoa and Objective-C](https://developer.apple.com/library/ios/documentation/Swift/Conceptual/BuildingCocoaApps/MixandMatch.html#//apple_ref/doc/uid/TP40014216-CH10-ID130).

## Use a module map to import C headers
A [**module map**](http://clang.llvm.org/docs/Modules.html#module-maps) can simply `import mymodule` by configuring it to read C header files and make them appear as Swift functions.

Place a file named `module.modulemap` inside a directory named `mymodule`:

<img src="http://i.stack.imgur.com/tchR7.png" width="200" alt="directory structure">

Inside the module map file:

    // mymodule/module.modulemap
    module mymodule {
        header "defs.h"
    }

Then `import` the module:

    // demo.swift
    import mymodule
    print("Empty color: \(Color())")

Use the <code>-I <i>directory</i></code> flag to tell `swiftc` where to find the module:

    swiftc -I . demo.swift   # "-I ." means "search for modules in the current directory"

For more information about the module map syntax, see the [Clang documentation about module maps](http://clang.llvm.org/docs/Modules.html#module-map-language).


## Using Objective-C classes from Swift code
If MyFramework contains Objective-C classes in its public headers (and the umbrella header), then **`import MyFramework`** is all that's necessary to use them from Swift.

## Bridging headers

A **bridging header** makes additional Objective-C and C declarations visible to Swift code. When adding project files, Xcode may offer to create a bridging header automatically:

![bridging header dialog][1]

To create one manually, modify the **Objective-C Bridging Header** build setting:

[![enter image description here][2]][2]


Inside the bridging header, import whichever files are necessary to use from code:

    // MyApp-Bridging-Header.h
    #import "MyClass.h"  // allows code in this module to use MyClass


## Generated Interface

Click the Related Items button (or press ⌃1), then select **Generated Interface** to see the Swift interface that will be generated from an Objective-C header.

<img src="http://i.stack.imgur.com/g6dL8.png" width="250"> &nbsp;&nbsp;&nbsp;&nbsp; <img src="http://i.stack.imgur.com/n2LeS.png" width="370">


  [1]: http://i.stack.imgur.com/marBX.png
  [2]: http://i.stack.imgur.com/MjsyZ.png

## Specify a bridging header to swiftc
The `-import-objc-header` flag specifies a header for `swiftc` to import:

    // defs.h
    struct Color {
        int red, green, blue;
    };
    
    #define MAX_VALUE 255


<!--language: lang-swift-->
    // demo.swift
    extension Color: CustomStringConvertible {  // extension on a C struct
        public var description: String {
            return "Color(red: \(red), green: \(green), blue: \(blue))"
        }
    }
    print("MAX_VALUE is: \(MAX_VALUE)")  // C macro becomes a constant
    let color = Color(red: 0xCA, green: 0xCA, blue: 0xD0)  // C struct initializer
    print("The color is \(color)")

<pre><code class="lang-none">$ swiftc demo.swift <b>-import-objc-header defs.h</b> && ./demo
MAX_VALUE is: 255
The color is Color(red: 202, green: 202, blue: 208)</code></pre>

## Fine-grained interoperation between Objective-C and Swift
When an API is marked with `NS_REFINED_FOR_SWIFT`, it will be prefixed with two underscores (`__`) when imported to Swift:

<pre><code>@interface MyClass : NSObject
- (NSInteger)indexOfObject:(id)obj <b>NS_REFINED_FOR_SWIFT</b>;
@end</code></pre>

The [generated interface](https://www.wikiod.com/swift/working-with-c-and-objective-c#Using Objective-C classes from Swift code) looks like this:

<pre><code>public class MyClass : NSObject {
    public func <b>__indexOfObject</b>(obj: AnyObject) -> Int
}</code></pre>

Now you can **replace the API** with a more "Swifty" extension. In this case, we can use an [optional](https://www.wikiod.com/swift/optionals) return value, filtering out [NSNotFound](https://developer.apple.com/library/mac/documentation/Cocoa/Reference/Foundation/Miscellaneous/Foundation_Constants/#//apple_ref/doc/constant_group/NSNotFound):

<pre><code>extension MyClass {
    // Rather than returning NSNotFound if the object doesn't exist,
    // this "refined" API returns nil.
    func <b>indexOfObject</b>(obj: AnyObject) -> <b>Int?</b> {
        let idx = __indexOfObject(obj)
        if idx == NSNotFound { return nil }
        return idx
    }
}

// Swift code, using "if let" as it should be:
let myobj = MyClass()
if let idx = myobj.indexOfObject(something) {
    // do something with idx
}
</code></pre>

In most cases you might want to restrict whether or not an argument to an Objective-C function could be `nil`. This is done using `_Nonnull` keyword, which qualifies any pointer or block reference:

<pre><code>void
doStuff(const void *const _Nonnull data, void (^_Nonnull completion)())
{
    // complex asynchronous code
}
</code></pre>

With that written, the compiler shall emit an error whenever we try to pass `nil` to that function from our Swift code:

<pre><code>doStuff(
    nil,  // error: nil is not compatible with expected argument type 'UnsafeRawPointer'
    nil)  // error: nil is not compatible with expected argument type '() -> Void'
</code></pre>

The opposite of `_Nonnull` is `_Nullable`, which means that it is acceptable to pass `nil` in this argument. `_Nullable` is also the default; however, specifying it explicitly allows for more self-documented and future-proof code.

To further help the compiler with optimising your code, you also might want to specify if the block is escaping:

<pre><code>void
callNow(__attribute__((noescape)) void (^_Nonnull f)())
{
    // f is not stored anywhere
}
</code></pre>

With this attribute we promise not to save the block reference and not to call the block after the function has finished execution.


## Use the C standard library
Swift's C interoperability allows you to use functions and types from the C standard library.

On Linux, the C standard library is exposed via the `Glibc` module; on Apple platforms it's called `Darwin`.

    #if os(macOS) || os(iOS) || os(tvOS) || os(watchOS)
    import Darwin
    #elseif os(Linux)
    import Glibc
    #endif

    // use open(), read(), and other libc features

## Using Swift classes from Objective-C code
## In the same module

Inside a module named "**MyModule**", Xcode generates a header named **`MyModule-Swift.h`** which exposes public Swift classes to Objective-C. Import this header in order to use the Swift classes:

<pre><code>// MySwiftClass.swift in MyApp
import Foundation

// The class must be `public` to be visible, unless this target also has a bridging header
<b>public</b> class MySwiftClass: NSObject {
    // ...
}</code></pre>


<pre><code>// MyViewController.m in MyApp

#import "MyViewController.h"
<b>#import "MyApp-Swift.h"</b>                    // import the generated interface
<b>#import &lt;MyFramework/MyFramework-Swift.h></b>  // or use angle brackets for a framework target

@implementation MyViewController
- (void)demo {
    [[MySwiftClass alloc] init];  // use the Swift class
}
@end</code></pre>

Relevant build settings:
- **Objective-C Generated Interface Header Name**: controls the name of the generated Obj-C header.
- **Install Objective-C Compatibility Header**: whether the -Swift.h header should be a public header (for framework targets).

[![build setting screenshot][1]][1]

----

## In another module

Using **`@import MyFramework;`** imports the whole module, including Obj-C interfaces to Swift classes (if the aforementioned build setting is enabled).

  [1]: http://i.stack.imgur.com/gpEa5.png

