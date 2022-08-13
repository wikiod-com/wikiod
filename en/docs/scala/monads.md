---
title: "Monads"
slug: "monads"
draft: false
images: []
weight: 9874
type: docs
toc: true
---

## Monad Definition
Informally, a monad is a container of elements, notated as `F[_]`, packed with 2 functions: `flatMap` (to transform this container) and `unit` (to create this container).

Common library examples include `List[T]`, `Set[T]` and `Option[T]`.

**Formal definition**

Monad `M` is a [parametric type][1] `M[T]` with two operations `flatMap` and `unit`, such as:
```
trait M[T] {
  def flatMap[U](f: T => M[U]): M[U]
}

def unit[T](x: T): M[T]
```
These functions must satisfy three laws:

 1. *Associativity*: `(m flatMap f) flatMap g = m flatMap (x => f(x) flatMap g)`
    <br/>That is, if the sequence is unchanged you may apply the terms in any order. Thus, applying `m` to `f`, and then applying the result to `g` will yield the same result as applying `f` to `g`, and then applying `m` to that result.
 2. *Left unit*:     `unit(x) flatMap f == f(x)`
    <br/>That is, the unit monad of `x` flat-mapped across `f` is equivalent to applying `f` to `x`. 
 3. *Right unit*:    `m flatMap unit == m`
    <br/>This is an 'identity': any monad flat-mapped against unit will return a monad equivalent to itself.

**Example**:

```
val m = List(1, 2, 3)
def unit(x: Int): List[Int] = List(x)
def f(x: Int): List[Int] = List(x * x)
def g(x: Int): List[Int] = List(x * x * x)
val x = 1
```
  1. *Associativity*: 
```
(m flatMap f).flatMap(g) == m.flatMap(x => f(x) flatMap g) //Boolean = true
//Left side:
List(1, 4, 9).flatMap(g) // List(1, 64, 729)
//Right side:
 m.flatMap(x => (x * x) * (x * x) * (x * x)) //List(1, 64, 729)

```
  2. *Left unit*
```
unit(x).flatMap(x => f(x)) == f(x)
List(1).flatMap(x => x * x) == 1 * 1

```
  3. *Right unit* 
```
//m flatMap unit == m
m.flatMap(unit) == m
List(1, 2, 3).flatMap(x => List(x)) == List(1,2,3) //Boolean = true
```
**Standard Collections are Monads**

Most of the standard collections are monads (`List[T]`, `Option[T]`), or monad-like (`Either[T]`, `Future[T]`). These collections can be easily combined together within `for` comprehensions (which are an equivalent way of writing `flatMap` transformations):

    val a = List(1, 2, 3)
    val b = List(3, 4, 5)
    for {
      i <- a
      j <- b
    } yield(i * j)

The above is equivalent to:

    a flatMap {
      i => b map {
        j => i * j
      }
    }

Because a monad preserves the *data structure* and only acts on the elements within that structure, we can endless chain monadic datastructures, as shown here in a for-comprehension.


  [1]: https://en.wikipedia.org/wiki/Parametric_polymorphism

