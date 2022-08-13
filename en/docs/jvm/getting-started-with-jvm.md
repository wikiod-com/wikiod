---
title: "Getting started with jvm"
slug: "getting-started-with-jvm"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation or Setup
Detailed instructions on getting jvm set up or installed.

## Enabling Parallel GC
Parallel GC is Stop-The-World (STW) collector which stop all the application threads when running the garbage collector. 

When Parallel GC was introduced it was only enabled the parallel GC in young generation collector and OldGeneration Collector was single thread stop-the-world collector, but later introduce separate command line option to enable the Old Parallel. 

Enable Parallel GC on Java 6 :
-XX:+UseParallelOldGC

Enable Parallel GC on Java 7u4 onward:
-XX:+UseParallelGC
OR 
-XX:+UseParallelOldGC

Parallel GC was made default on Java7 update 4 onward , however specifying the one of above option makes it explicit.


