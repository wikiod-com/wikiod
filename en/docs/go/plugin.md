---
title: "Plugin"
slug: "plugin"
draft: false
images: []
weight: 9985
type: docs
toc: true
---

Go provides a plugin mechanism that can be used to dynamically link other Go code at runtime.

As of Go 1.8, it is only usable on Linux.

## Defining and using a plugin
```
package main

import "fmt"

var V int

func F() { fmt.Printf("Hello, number %d\n", V) }
```

This can be built with:

```
go build -buildmode=plugin
```

And then loaded and used from your application:

```
p, err := plugin.Open("plugin_name.so")
if err != nil {
    panic(err)
}

v, err := p.Lookup("V")
if err != nil {
    panic(err)
}

f, err := p.Lookup("F")
if err != nil {
    panic(err)
}

*v.(*int) = 7
f.(func())() // prints "Hello, number 7"
```

Example from _[The State of Go 2017](https://talks.golang.org/2017/state-of-go.slide#1)_.

