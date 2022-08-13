---
title: "JVM Heap Dump"
slug: "jvm-heap-dump"
draft: false
images: []
weight: 9985
type: docs
toc: true
---

## Generating heap dump upon OutOfMemoryError
**Note:** This example is based on the Oracle JVM implementation.

Built-in tools like `jmap`, `jconsole`, and `jvisualvm` are available in a JDK and can be used to generate and analyze heap memory dumps taken from a running JVM application. However, one option to generate a heap dump without using JDK tools is to add the VM argument `-XX:+HeapDumpOnOutOfMemoryError` which tells the JVM to automatically generate a heap dump when an OutOfMemoryError occurs, and the argument `-XX:HeapDumpPath` to specify the path for the heap dump.

Also see: [Java HotSpot VM Options](http://www.oracle.com/technetwork/articles/java/vmoptions-jsp-140102.html), specifically:

>**-XX:HeapDumpPath=./java_pid<pid>.hprof**     Path to directory or filename for heap dump. Manageable. (Introduced in 1.4.2 update 12, 5.0 update 7.)

>**-XX:-HeapDumpOnOutOfMemoryError**     Dump heap to file when java.lang.OutOfMemoryError is thrown. Manageable. (Introduced in 1.4.2 update 12, 5.0 update 7.)

If a concurrent collector such as CMS or G1 are used then a FullGC can be considered a failure mode and using `HeapDumpBeforeFullGC` or `HeapDumpAfterFullGC` can be useful to diagnose them.


