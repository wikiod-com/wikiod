---
title: "Optimization"
slug: "optimization"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

## Always measure first!
These are general tips that in general improve performance. If your code is slow, it is always important to profile it to figure out what parts are slow. Guessing is **never** enough. Improving the execution speed of something that only takes up 1% of the execution time probably isn't worth the effort. Look for the big time sinks. 

To get somewhat accurate numbers, make sure the code you are optimizing is executed for at least one second when profiling. If you spend 10% of the execution time in that function, make sure the complete program execution takes up at least 10 seconds, and make sure you can run the same exact data through the code multiple times, to get repeatable numbers. 

[ExProf][1] is simple to get started with.


  [1]: https://github.com/parroty/exprof

