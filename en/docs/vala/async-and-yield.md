---
title: "Async and Yield"
slug: "async-and-yield"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

Vala provide two syntax constructs to deal with asynchonous operations: `async` function and `yield` statement.

## Declare an Asynchronous Function
<!-- language: lang-vala -->

    public async int call_async () {
        return 1;
    }
    
    call_async.begin ((obj, res) => {
        var ret = call_async.end (res);
    });

To call an asynchronous functions from a synchronous context, use the `begin` method and pass a callback to receive the result. The two arguments are:

 - `obj` is a `GLib.Object` if this call was defined in a class
 - `res` is a `GLib.AsyncResult` holding the result of the asynchronous operation

The `end` method extract the result of the operation.

## Usage of GLib.Task to perform asynchronous operations
The `GLib.Task` provide low-level API for performing asynchronous operations.

<!-- language: lang-vala -->

    var task = new GLib.Task (null, null, (obj, result) => {
        try {
            var ret = result.propagate_boolean ();
        } catch (Error err) {
            // handler err...
        }
    });

Later in a thread or a callback:

<!-- language: lang-vala -->

    task.return_boolean (true);

To use the `GLib.Task` internal thread pool:

<!-- language: lang-vala -->

    task.run_in_thread (() => {
        task.return_boolean (true);
    });



## Yield from an Asynchronous Function
In order to chain asynchronous operations and avoid a callback hell, Vala supports the `yield` statement.

Used with an asynchronous invocation, it will pause the current coroutine until the call is completed and extract the result.

Used alone, `yield` pause the current coroutine until it's being woken up by invoking its source callback.

<!-- lang: vala -->

    public async int foo_async () {
        yield; // pause the coroutine
        Timeout.add_seconds (5, bar_async.callback); // wakeup in 5 seconds
        return ret + 10;
    }

    public async int bar_async () {
        var ret = yield foo_async ();
    }

