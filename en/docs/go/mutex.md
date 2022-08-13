---
title: "Mutex"
slug: "mutex"
draft: false
images: []
weight: 9980
type: docs
toc: true
---

## Mutex Locking
Mutex locking in Go allows you to ensure that only one goroutine at a time has a lock:

    import "sync"
    
    func mutexTest() {
        lock := sync.Mutex{}
        go func(m *sync.Mutex) {
            m.Lock()
            defer m.Unlock()   // Automatically unlock when this function returns
            // Do some things
        }(&lock)

        lock.Lock()
        // Do some other things
        lock.Unlock()
    }

Using a `Mutex` allows you to avoid race conditions, concurrent modifications, and other issues associated with multiple concurrent routines operating on the same resources. Note that `Mutex.Unlock()` can be executed by any routine, not just the routine that got the lock. Also note that the call to `Mutex.Lock()` will not fail if another routine holds the lock; it will block until the lock is released.

**Tip:** Whenever you're passing a Mutex variable to a function, always pass it as a pointer. Otherwise a copy is made of your variable, which defeats the purpose of the Mutex. If you're using an older Go version (< 1.7), the compiler will not warn you about this mistake!

