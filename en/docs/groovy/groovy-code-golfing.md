---
title: "Groovy code golfing"
slug: "groovy-code-golfing"
draft: false
images: []
weight: 9996
type: docs
toc: true
---

Tips for golfing in Groovy

## Spread dot operator(*.)
Spread dot operator can be used instead of collect method

    (1..10)*.multiply(2) // equivalent to (1..10).collect{ it *2 }
    d = ["hello", "world"]
    d*.size() // d.collect{ it.size() }

## Parallel processing using Gpars
Gpars offers intuitive ways to handle tasks concurrently

    import groovyx.gpars.*
    GParsPool.withPool { def result = dataList.collectParallel { processItem(it) } }

