---
title: "Memory pooling"
slug: "memory-pooling"
draft: false
images: []
weight: 9971
type: docs
toc: true
---

sync.Pool stores a cache of allocated but unused items for future use, avoiding memory churn for frequently changed collections, and allowing efficient, thread-safe re-use of memory. It is useful to manage a group of temporary items shared between concurrent clients of a package, for example a list of database connections or a list of output buffers. 

## sync.Pool
Using [`sync.Pool`][1] structure we can pool objects and reuse them.
 
 

    package main

    import (
        "bytes"
        "fmt"
        "sync"
    )
    
    var pool = sync.Pool{
        // New creates an object when the pool has nothing available to return.
        // New must return an interface{} to make it flexible. You have to cast
        // your type after getting it.
        New: func() interface{} {
            // Pools often contain things like *bytes.Buffer, which are
            // temporary and re-usable.
            return &bytes.Buffer{}
        },
    }
    
    func main() {
        // When getting from a Pool, you need to cast
        s := pool.Get().(*bytes.Buffer)
        // We write to the object
        s.Write([]byte("dirty"))
        // Then put it back
        pool.Put(s)
    
        // Pools can return dirty results
    
        // Get 'another' buffer
        s = pool.Get().(*bytes.Buffer)
        // Write to it
        s.Write([]bytes("append"))
        // At this point, if GC ran, this buffer *might* exist already, in
        // which case it will contain the bytes of the string "dirtyappend"
        fmt.Println(s)
        // So use pools wisely, and clean up after yourself
        s.Reset()
        pool.Put(s)
    
        // When you clean up, your buffer should be empty
        s = pool.Get().(*bytes.Buffer)
        // Defer your Puts to make sure you don't leak!
        defer pool.Put(s)
        s.Write([]byte("reset!"))
        // This prints "reset!", and not "dirtyappendreset!"
        fmt.Println(s)
    }


 [1]: https://golang.org/pkg/sync/#Pool

