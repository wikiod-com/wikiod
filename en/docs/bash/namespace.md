---
title: "Namespace"
slug: "namespace"
draft: false
images: []
weight: 9976
type: docs
toc: true
---

## There are no such things as namespaces
    myfunc(){
        echo "I will never be executed."
    }
    another_func(){
        # this "redeclare" overwrites original function
        myfunc(){ echo "I am the one and only"; }
    }
    # myfunc will print "I will never be executed"
    myfunc
    # but if we call another_func first
    another_func
    # it gets overwritten and
    myfunc
    # no prints "I am the one and only"

The latest declaration wins. There are no such things as namespaces!
However, functions can contain other functions.

