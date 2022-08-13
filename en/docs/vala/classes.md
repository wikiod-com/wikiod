---
title: "Classes"
slug: "classes"
draft: false
images: []
weight: 9991
type: docs
toc: true
---

Vala support various flavours of classes.

Both `glib-2.0` and `gobject-2.0` dependencies are required unless `--nostdpkg` is explicitly given.

## Compact Class
<!-- language: lang-vala -->

    [Compact]
    public class Foo {
        public string prop;
    }

It is mainly used for writing bindings with specific memory management.


## GObject Class
<!-- language: lang-vala -->
```
public class Foo : Object {
    public string prop { construct; get; }
}
```

It is meant for interospectable API using GObject Introspection. This is the recommended way for declaring classes.

## Plain Class
<!-- language: lang-vala -->
```
public class Foo {
    public string prop { construct; get; }
}
```

Pure-Vala and lightweight class. This is useful if you need a compromise between efficiency of a `struct` and the feature of a full blown GObject class.

