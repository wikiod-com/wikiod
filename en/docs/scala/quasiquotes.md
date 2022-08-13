---
title: "Quasiquotes"
slug: "quasiquotes"
draft: false
images: []
weight: 9984
type: docs
toc: true
---

## Create a syntax tree with quasiquotes
Use quasiquotes to create a `Tree` in a macro.

```
object macro {
  def addCreationDate(): java.util.Date = macro impl.addCreationDate
}

object impl {
  def addCreationDate(c: Context)(): c.Expr[java.util.Date] = {
    import c.universe._

    val date = q"new java.util.Date()" // this is the quasiquote
    c.Expr[java.util.Date](date)
  }
}
```

It can be arbitrarily complex but it will be validated for correct scala syntax.

