---
title: "Reflection"
slug: "reflection"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

## Loading a class using reflection
     import scala.reflect.runtime.universe._
     val mirror = runtimeMirror(getClass.getClassLoader)
     val module = mirror.staticModule("org.data.TempClass")

