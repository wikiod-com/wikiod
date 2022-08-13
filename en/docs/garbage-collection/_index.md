---
title : garbage-collection Tutorial
slug : garbage-collection-tutorial
weight : 9992
draft : false
images : []
type : docs
---

Garbage Collection (GC) is a way of automatically reclaiming memory that is occupied by objects that are no longer needed by a program. This is in contrast with manual memory management where the programmer explicitly specifies which objects should be deallocated and returned to memory. Good GC-strategies can be more efficient than manual memory management, but it may depend on the type of software.

The main advantages of garbage collection are:

 - It frees the programmer from having to do doing manual memory management.
 - It avoids certain difficult-to-find bugs that may arise from manual memory management (e.g. dangling pointers, double freeing, certain types of memory leaks).
 - Languages that use garbage collection are usually less complex.

The main disadvantages are:
 
 - Garbage collection has some overhead compared to manual memory management.
 - It can potentially impact performance, especially when garbage collection is triggered at undesirable moments.
 - It is indeterministic, the programmer doesn't know when garbage collection is done and if objects are freed or not.

Most 'newer' programming languages have garbage collection built-in, for example Java, C#, .NET, Ruby and JavaScript.
Older languages like C and C++ do not have garbage collection although there are implementations available with garbage collection. There are also languages that allow you to use a combination of garbage collection and manual memory management, for example Modula-3 and Ada.

Garbage collection strategies differ but many use a (variation of) the mark-and-sweep approach. In the mark phase all accessible objects are found and marked. In the sweep phase the heap is scanned for inaccessible and unmarked objects which are then cleaned up.
Modern garbage collectors also use a generational approach where two or more object allocation regions (generations) are kept. The youngest generation contains the newest allocated objects and is cleaned more often. Objects that 'survive' for a certain timespan are promoted to an older generation. 

Many languages with GC allow programmers to fine-tune it (see for example the [Java 8 Virtual Machine Garbage Collection Tuning Guide](http://docs.oracle.com/javase/8/docs/technotes/guides/vm/gctuning/) or the [.Net Garbage Collection documentation](https://msdn.microsoft.com/en-us/library/0xy59wtx.aspx))




