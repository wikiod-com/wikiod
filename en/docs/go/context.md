---
title: "Context"
slug: "context"
draft: false
images: []
weight: 9921
type: docs
toc: true
---

## Syntax
- type CancelFunc func()
- func Background() Context
- func TODO() Context
- func WithCancel(parent Context) (ctx Context, cancel CancelFunc)
- func WithDeadline(parent Context, deadline time.Time) (Context, CancelFunc)
- func WithTimeout(parent Context, timeout time.Duration) (Context, CancelFunc)
- func WithValue(parent Context, key interface{}, val interface{})

The `context` package (in Go 1.7) or the `golang.org/x/net/context` package (Pre 1.7) is an interface for creating contexts that can be used to carry request scoped values and deadlines across API boundaries and between services, as well as a simple implementation of said interface.

aside: the word "context" is loosely used to refer to the entire tree, or to individual leaves in the tree, eg. the actual `context.Context` values.

At a high level, a context is a tree. New leaves are added to the tree when they are constructed (a `context.Context` with a parent value), and leaves are never removed from the tree. Any context has access to all of the values above it (data access only flows upwards), and if any context is canceled its children are also canceled (cancelation signals propogate downwards). The cancel signal is implemented by means of a function that returns a channel which will be closed (readable) when the context is canceled; this makes contexts a very efficient way to implement the [pipeline and cancellation concurrency pattern](https://blog.golang.org/pipelines), or timeouts.

By convention, functions that take a context have the first argument `ctx context.Context`. While this is just a convention, it's one that should be followed since many static analysis tools specifically look for this argument. Since Context is an interface, it's also possible to turn existing context-like data (values that are passed around throughout a request call chain) into a normal Go context and use them in a backwards compatible way just by implementing a few methods. Furthermore, contexts are safe for concurrent access so you can use them from many goroutines (whether they're running on parallel threads or as concurrent coroutines) without fear.

## Further Reading ##

 - https://blog.golang.org/context

## Context tree represented as a directed graph
A simple context tree (containing some common values that might be request scoped and included in a context) built from Go code like the following:

    // Pseudo-Go
    ctx := context.WithValue(
        context.WithDeadline(
            context.WithValue(context.Background(), sidKey, sid),
            time.Now().Add(30 * time.Minute),
        ),
        ridKey, rid,
    )
    trCtx := trace.NewContext(ctx, tr)
    logCtx := myRequestLogging.NewContext(ctx, myRequestLogging.NewLogger())

Is a tree that can be represented as a directed graph that looks like this:

[![Context represented as a directed graph][1]][1]

Each child context has access to the values of its parent contexts, so the data access flows upwards in the tree (represented by black edges). Cancelation signals on the other hand travel down the tree (if a context is canceled, all of its children are also canceled). The cancelation signal flow is represented by the grey edges.

  [1]: http://i.stack.imgur.com/R0CED.png


## Using a context to cancel work
Passing a context with a timeout (or with a cancel function) to a long running function can be used to cancel that functions work:

    ctx, _ := context.WithTimeout(context.Background(), 200*time.Millisecond)
    for {
        select {
        case <-ctx.Done():
            return ctx.Err()
        default:
            // Do an iteration of some long running work here!
        }
    }

