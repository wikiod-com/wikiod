---
title: "Memory Management"
slug: "memory-management"
draft: false
images: []
weight: 9825
type: docs
toc: true
---

This topic outlines how and when the Swift runtime shall allocate memory for application data structures, and when that memory shall be reclaimed.

By default, the memory backing class instances is managed through reference counting. The structures are always passed through copying. To opt out of the built-in memory management scheme, one could use [`Unmanaged`][1] structure.

  [1]: https://developer.apple.com/reference/swift/unmanaged


<h2>When to use the weak-keyword:</h2>
The `weak`-keyword should be used, if a referenced object may be deallocated during the lifetime of the object holding the reference.

<h2>When to use the unowned-keyword:</h2>
The `unowned`-keyword should be used, if a referenced object is not expected to be deallocated during the lifetime of the object holding the reference. 

<h2>Pitfalls</h2>
A frequent error is to forget to create references to objects, which are required to live on after a function ends, like location managers, motion managers, etc.

Example:
    
    class A : CLLocationManagerDelegate
    {
        init()
        {
            let locationManager = CLLocationManager()
            locationManager.delegate = self
            locationManager.startLocationUpdates()
        }
    }

This example will not work properly, as the location manager is deallocated after the initializer returns. The proper solution is to create a strong reference as an instance variable:
    
    class A : CLLocationManagerDelegate
    {
        let locationManager:CLLocationManager

        init()
        {
            locationManager = CLLocationManager()
            locationManager.delegate = self
            locationManager.startLocationUpdates()
        }
    }


## Reference Cycles and Weak References
A *reference cycle* (or *retain cycle*) is so named because it indicates a [cycle](https://en.wikipedia.org/wiki/Cycle_(graph_theory)) in the [object graph](https://en.wikipedia.org/wiki/Object_graph):

<img src="http://i.stack.imgur.com/wfapB.png" width="400" alt="retain cycle">

Each arrow indicates one object [retaining](https://developer.apple.com/library/ios/documentation/Swift/Conceptual/Swift_Programming_Language/AutomaticReferenceCounting.html) another (a strong reference). Unless the cycle is broken, the memory for these objects will **never be freed**.

A retain cycle is created when two instances of classes reference each other:

    class A { var b: B? = nil }
    class B { var a: A? = nil }

    let a = A()
    let b = B()

    a.b = b  // a retains b
    b.a = a  // b retains a -- a reference cycle

Both instances they will live on until the program terminates. This is a retain cycle.

# Weak References
To avoid retain cycles, use the keyword `weak` or `unowned` when creating references to break retain cycles.

<pre><code>class B { <b>weak</b> var a: A? = nil }</code></pre>

Weak or unowned references will not increase the reference count of an instance. These references don't contribute to retain cycles. The weak reference **becomes `nil`** when the object it references is deallocated.

    a.b = b  // a retains b
    b.a = a  // b holds a weak reference to a -- not a reference cycle

When working with closures, you can also use [`weak` and `unowned` in capture lists](https://www.wikiod.com/swift/closures#Captures, strong/weak references, and retain cycles).

## Manual Memory Management
When interfacing with C APIs, one might want to back off Swift reference counter. Doing so is achieved with unmanaged objects.

If you need to supply a type-punned pointer to a C function, use `toOpaque` method of the `Unmanaged` structure to obtain a raw pointer, and `fromOpaque` to recover the original instance:

    setupDisplayLink() {
      let pointerToSelf: UnsafeRawPointer = Unmanaged.passUnretained(self).toOpaque()
      CVDisplayLinkSetOutputCallback(self.displayLink, self.redraw, pointerToSelf)
    }

    func redraw(pointerToSelf: UnsafeRawPointer, /* args omitted */) {
      let recoveredSelf = Unmanaged<Self>.fromOpaque(pointerToSelf).takeUnretainedValue()
      recoveredSelf.doRedraw()
    }

Note that, if using `passUnretained` and counterparts, it's necessary to take all precautions as with `unowned` references.

To interact with legacy Objective-C APIs, one might want to manually affect reference count of a certain object. For that `Unmanaged` has respective methods `retain` and `release`. Nonetheless, it is more desired to use `passRetained` and `takeRetainedValue`, which perform retaining before returning the result:

    func preferredFilenameExtension(for uti: String) -> String! {
      let result = UTTypeCopyPreferredTagWithClass(uti, kUTTagClassFilenameExtension)
      guard result != nil else { return nil }

      return result!.takeRetainedValue() as String
    }

These solutions should always be the last resort, and language-native APIs sould always be preferred.

