---
title: "JVM Heap"
slug: "jvm-heap"
draft: false
images: []
weight: 9983
type: docs
toc: true
---

## Setting the maximum heap size.
Most JVMs have an option to set the maximum heap size e.g.

    -Xmx64m
    -Xmx8g

In Java 1.0 to 1.2 you could use

    -mx64m

and this is still available on some JVMs for backward compatibility (E.g. Oracle JVM).

There are a few common misconceptions about this setting.

- It doesn't set the heap size, only the maximum. `-Xms` sets the initial heap size.
- It doesn't set the amount of memory the JVM will use.  While the heap is an important area of memory, there are many other regions for code Perm Gen/Metaspace, thread stacks, GUI components, direct memory etc.

The amount of memory used at run time can change dynamically.

## Specify heap region size
The latest JVMs supports Garbage First GC ( G1 GC) and consists of set of regions which accumulate to make young and old generation.

The JVM will have approximately 2048 reagions and set heap region size accordingly from 1 MB to 32 MB and power of 2 bounds. This is important parameter which decide what size of object that can be store in a region.

Heap region size = Heap size/2048

you can overwrite the adaptive selection of the region size by comand line 
JVM paramter  -XX:G1HeapRegionSize=n

