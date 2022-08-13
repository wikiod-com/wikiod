---
title: "Closures"
slug: "closures"
draft: false
images: []
weight: 9725
type: docs
toc: true
---

## Syntax
- var closureVar: (\<parameters>) -> (\<returnType>) // As a variable or property type
- typealias ClosureType = (\<parameters>) -> (\<returnType>)
- { [\<captureList>] (\<parameters>) \<throws-ness> -> \<returnType> in \<statements> } // Complete closure syntax

For more information on Swift closures, see [Apple's documentation](https://developer.apple.com/library/prerelease/content/documentation/Swift/Conceptual/Swift_Programming_Language/Closures.html).

## Closure basics
**[Closures](https://developer.apple.com/library/ios/documentation/Swift/Conceptual/Swift_Programming_Language/Closures.html)** (also known as **blocks** or **lambdas**) are pieces of code which can be stored and passed around within your program.

    let sayHi = { print("Hello") }
    // The type of sayHi is "() -> ()", aka "() -> Void"

    sayHi()  // prints "Hello"

Like other functions, closures can accept arguments and return results or throw [errors](https://www.wikiod.com/swift/error-handling):

    let addInts = { (x: Int, y: Int) -> Int in
        return x + y
    }
    // The type of addInts is "(Int, Int) -> Int"

    let result = addInts(1, 2)  // result is 3

    let divideInts = { (x: Int, y: Int) throws -> Int in
        if y == 0 {
            throw MyErrors.DivisionByZero
        }
        return x / y
    }
    // The type of divideInts is "(Int, Int) throws -> Int"

Closures can **capture** values from their scope:

    // This function returns another function which returns an integer
    func makeProducer(x: Int) -> (() -> Int) {
        let closure = { x }  // x is captured by the closure
        return closure
    }

    // These two function calls use the exact same code,
    // but each closure has captured different values.
    let three = makeProducer(3)
    let four = makeProducer(4)
    three()  // returns 3
    four()  // returns 4

Closures can be passed directly into functions:

    let squares = (1...10).map({ $0 * $0 })  // returns [1, 4, 9, 16, 25, 36, 49, 64, 81, 100]
    let squares = (1...10).map { $0 * $0 }

    NSURLSession.sharedSession().dataTaskWithURL(myURL,
        completionHandler: { (data: NSData?, response: NSURLResponse?, error: NSError?) in
            if let data = data {
                print("Request succeeded, data: \(data)")
            } else {
                print("Request failed: \(error)")
            }
        }).resume()

## Syntax variations
The basic closure syntax is

> `{` `[`*capture list*`]` `(`*parameters*`)` *throws-ness* `-> ` *return type* `in` *body* `}`.

Many of these parts can be omitted, so there are several equivalent ways to write simple closures:

    let addOne = { [] (x: Int) -> Int in return x + 1 }
    let addOne = { [] (x: Int) -> Int in x + 1 }
    let addOne = { (x: Int) -> Int in x + 1 }
    let addOne = { x -> Int in x + 1 }
    let addOne = { x in x + 1 }
    let addOne = { $0 + 1 }

    let addOneOrThrow = { [] (x: Int) throws -> Int in return x + 1 }
    let addOneOrThrow = { [] (x: Int) throws -> Int in x + 1 }
    let addOneOrThrow = { (x: Int) throws -> Int in x + 1 }
    let addOneOrThrow = { x throws -> Int in x + 1 }
    let addOneOrThrow = { x throws in x + 1 }

- The capture list can be omitted if it's empty.
- Parameters don't need type annotations if their types can be inferred.
- The return type doesn't need to be specified if it can be inferred.
- Parameters don't have to be named; instead they can be referred to with `$0`, `$1`, `$2`, etc.
- If the closure contains a single expression, whose value is to be returned, the `return` keyword can be omitted.
- If the closure is inferred to throw an error, is written in a context which expects a throwing closure, or doesn't throw an error, `throws` can be omitted.


    // The closure's type is unknown, so we have to specify the type of x and y.
    // The output type is inferred to be Int, because the + operator for Ints returns Int.
    let addInts = { (x: Int, y: Int) in x + y }

    // The closure's type is specified, so we can omit the parameters' type annotations.
    let addInts: (Int, Int) -> Int = { x, y in x + y }
    let addInts: (Int, Int) -> Int = { $0 + $1 }

## Passing closures into functions
Functions may accept closures (or other functions) as parameters:

    func foo(value: Double, block: () -> Void) { ... }
    func foo(value: Double, block: Int -> Int) { ... }
    func foo(value: Double, block: (Int, Int) -> String) { ... }

## Trailing closure syntax

If a function's last parameter is a closure, the closure braces `{`/`}` may be written **after** the function invocation:

    foo(3.5, block: { print("Hello") })

    foo(3.5) { print("Hello") }

    dispatch_async(dispatch_get_main_queue(), {
        print("Hello from the main queue")
    })

    dispatch_async(dispatch_get_main_queue()) {
        print("Hello from the main queue")
    }

If a function's only argument is a closure, you may also omit the pair of parentheses `()` when calling it with the trailing closure syntax:

    func bar(block: () -> Void) { ... }

<b></b>

    bar() { print("Hello") }
    
    bar { print("Hello") }

---
## `@noescape` parameters

Closure parameters marked `@noescape` are guaranteed to execute before the function call returns, so using `self.` is not required inside the closure body:

<pre><code>func executeNow(<b>@noescape</b> block: () -> Void) {
    // Since `block` is @noescape, it's illegal to store it to an external variable.
    // We can only call it right here.
    block()
}

func executeLater(block: () -> Void) {
    dispatch_async(dispatch_get_main_queue()) {
        // Some time in the future...
        block()
    }
}</code></pre>

    class MyClass {
        var x = 0
        func showExamples() {
            // error: reference to property 'x' in closure requires explicit 'self.' to make capture semantics explicit
            executeLater { x = 1 }

            executeLater { self.x = 2 }  // ok, the closure explicitly captures self

            // Here "self." is not required, because executeNow() takes a @noescape block.
            executeNow { x = 3 }

            // Again, self. is not required, because map() uses @noescape.
            [1, 2, 3].map { $0 + x }
        }
    }

### Swift 3 note:

Note that in Swift 3, you no longer mark blocks as @noescape. Blocks are now **not** escaping by default.
In Swift 3, instead of marking a closure as non-escaping, you mark a function parameter that is an escaping closure as escaping using the "@escaping" keyword.

---
## `throws` and `rethrows`

Closures, like other functions, may throw [errors](https://www.wikiod.com/swift/error-handling):

<pre><code>func executeNowOrIgnoreError(block: () <b>throws</b> -> Void) {
    do {
        try block()
    } catch {
        print("error: \(error)")
    }
}</code></pre>

The function may, of course, pass the error along to its caller:

<pre><code>func executeNowOrThrow(block: () <b>throws</b> -> Void) <b>throws</b> {
    try block()
}</code></pre>

However, if the block passed in *doesn't* throw, the caller is still stuck with a throwing function:

    // It's annoying that this requires "try", because "print()" can't throw!
    try executeNowOrThrow { print("Just printing, no errors here!") }

The solution is **`rethrows`**, which designates that the function can only throw **if its closure parameter throws**:

<pre><code>func executeNowOrRethrow(block: () <b>throws</b> -> Void) <b><i>rethrows</i></b> {
    try block()
}

// "try" is not required here, because the block can't throw an error.
executeNowOrRethrow { print("No errors are thrown from this closure") }

// This block <b>can</b> throw an error, so "try" is required.
try executeNowOrRethrow { throw MyError.Example }</code></pre>

Many standard library functions use `rethrows`, including `map()`, `filter()`, and `indexOf()`.

## Captures, strong/weak references, and retain cycles
    class MyClass {
        func sayHi() { print("Hello") }
        deinit { print("Goodbye") }
    }

When a closure captures a reference type (a class instance), it holds a strong reference by default:

    let closure: () -> Void
    do {
        let obj = MyClass()
        // Captures a strong reference to `obj`: the object will be kept alive
        // as long as the closure itself is alive.
        closure = { obj.sayHi() }
        closure()  // The object is still alive; prints "Hello"
    } // obj goes out of scope
    closure()  // The object is still alive; prints "Hello"

The closure's **capture list** can be used to specify a weak or unowned reference:

<pre><code>let closure: () -> Void
do {
    let obj = MyClass()
    // Captures a weak reference to `obj`: the closure will not keep the object alive;
    // the object becomes optional inside the closure.
    closure = { [<b>weak</b> obj] in obj?.sayHi() }
    closure()  // The object is still alive; prints "Hello"
} // obj goes out of scope and is deallocated; prints "Goodbye"
closure()  // `obj` is nil from inside the closure; this does not print anything.</code></pre>

<pre><code>let closure: () -> Void
do {
    let obj = MyClass()
    // Captures an unowned reference to `obj`: the closure will not keep the object alive;
    // the object is always <b>assumed</b> to be accessible while the closure is alive.
    closure = { [<b>unowned</b> obj] in obj.sayHi() }
    closure()  // The object is still alive; prints "Hello"
} // obj goes out of scope and is deallocated; prints "Goodbye"
closure()  // <b>crash!</b> obj is being accessed after it's deallocated.</code></pre>

For more information, see the [Memory Management](https://www.wikiod.com/swift/memory-management) topic, and the [Automatic Reference Counting](https://developer.apple.com/library/ios/documentation/Swift/Conceptual/Swift_Programming_Language/AutomaticReferenceCounting.html) section of The Swift Programming Language.

# Retain cycles

If an object holds onto a closure, which also holds a strong reference to the object, this is a [**retain cycle**](https://www.wikiod.com/swift/memory-management#Reference Cycles and Weak References). Unless the cycle is broken, the memory storing the object and closure will be leaked (never reclaimed).

<pre><code>class Game {
    var score = 0
    let controller: GCController
    init(controller: GCController) {
        self.controller = controller

        // <b>BAD:</b> the block captures self strongly, but self holds the controller
        // (and thus the block) strongly, which is a cycle.
        self.controller.controllerPausedHandler = {
            let curScore = self.score
            print("Pause button pressed; current score: \(curScore)")
        }

        // <b>SOLUTION:</b> use `weak self` to break the cycle.
        self.controller.controllerPausedHandler = { <b>[weak self] in</b>
            <b>guard let strongSelf = self else { return }</b>
            let curScore = strongSelf.score
            print("Pause button pressed; current score: \(curScore)")
        }
    }
}</code></pre>

## Using closures for asynchronous coding
Closures are often used for asynchronous tasks, for example when fetching data from a website.

<!-- if version [lt 3.0] -->
    func getData(urlString: String, callback: (result: NSData?) -> Void) {

        // Turn the URL string into an NSURLRequest.
        guard let url = NSURL(string: urlString) else { return }
        let request = NSURLRequest(URL: url)
        
        // Asynchronously fetch data from the given URL.
        let task = NSURLSession.sharedSession().dataTaskWithRequest(request) {(data: NSData?, response: NSURLResponse?, error: NSError?) in

            // We now have the NSData response from the website.
            // We can get it "out" of the function by using the callback 
            // that was passed to this function as a parameter.
    
            callback(result: data)
        }
            
        task.resume()
    }
<!-- end version if -->

This function is asynchronous, so will not block the thread it is being called on (it won't freeze the interface if called on the main thread of your GUI application).

<!-- if version [lt 3.0] -->
    print("1. Going to call getData")
    
    getData("http://www.example.com") {(result: NSData?) -> Void in

        // Called when the data from http://www.example.com has been fetched.
        print("2. Fetched data")
    }

    print("3. Called getData")
<!-- end version if -->

Because the task is asynchronous, the output will usually look like this:

    "1. Going to call getData"
    "3. Called getData"
    "2. Fetched data"

Because the code inside of the closure, `print("2. Fetched data")`, will not be called until the data from the URL is fetched.

## Closures and Type Alias
A closure can be defined with a `typealias`. This provides a convenient type placeholder if the same closure signature is used in multiple places. For example, common network request callbacks or user interface event handlers make great candidates for being "named" with a type alias.

<pre><code>public typealias <b>ClosureType</b> = (x: Int, y: Int) -> Int</code></pre>

You can then define a function using the typealias:

<pre><code>public func closureFunction(closure: <b>ClosureType</b>) {
    let z = closure(1, 2)
}
    
closureFunction() { (x: Int, y: Int) -> Int in return x + y }</code></pre>

