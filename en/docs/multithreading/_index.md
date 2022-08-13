---
title : multithreading Tutorial
slug : multithreading-tutorial
weight : 9924
draft : false
images : []
type : docs
---

Multithreading is a programming technique which consists of dividing a task into separate threads of execution. These threads run concurrently, either by being assigned to different processing cores, or by time-slicing.

When designing a multithreaded program, the threads should be made as independent of each other as possible, to achieve the greatest speed-up.  
In practice the threads are rarely fully independent, which makes synchronisation  necessary.  
The maximum theoretical speed-up can be calculated using [Amdahl's law](https://en.wikipedia.org/wiki/Amdahl%27s_law).

**Advantages**
* Speed up execution time by using the available processing resources efficiently
* Allow a process to remain responsive without the need to split lengthy calculations or expensive I/O operations
* Easily prioritize certain operations over others

**Disadvantages**
* Without careful design, hard-to-find bugs may be introduced
* Creating threads involves some overhead


