---
title: "Module System"
slug: "module-system"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

## Lazy evaluation
Standard ML doesn't have built-in support for lazy evaluation. Some implementations, notably SML/NJ, have nonstandard lazy evaluation primitives, but programs that use those primitives won't be portable. Lazy suspensions can also be implemented in a portable manner, using Standard ML's module system.

First we define an interface, or *signature*, for manipulating lazy suspensions:

```
signature LAZY =
sig
  type 'a lazy
  
  val pure : 'a -> 'a lazy
  val delay : ('a -> 'b) -> 'a -> 'b lazy
  val force : 'a lazy -> 'a
  
  exception Diverge
  
  val fix : ('a lazy -> 'a) -> 'a
end
```

This signature indicates that:

* The type constructor of lazy suspensions is *abstract* - its internal representation is hidden from (and irrelevant to) users.
* There are two ways to create a suspension: by directly wrapping its final result, and by delaying a function application.
* The only thing we can do with a suspension is force it. When a delayed suspension is forced for the first time, its result is memoized, so that the next time the result won't have to be recomputed.
* We can create self-referential values, where the self-reference goes through a suspension. This way we can create, for example, a logically infinite stream containing the same repeated element, as in the following Haskell snippet:

```
-- Haskell, not Standard ML!
xs :: [Int]
xs = 1 : xs
```

---

After defining the interface, we have to provide an actual implementation, also known as module or *structure*:

```
structure Lazy :> LAZY =
struct
  datatype 'a state
    = Pure of 'a
    | Except of exn
    | Delay of unit -> 'a
  
  type 'a lazy = 'a state ref
  
  fun pure x = ref (Pure x)
  fun delay f x = ref (Delay (fn _ => f x))
  fun compute f = Pure (f ()) handle e => Except e
  fun force r =
    case !r of
        Pure x => x
      | Except e => raise e
      | Delay f => (r := compute f; force r)
  
  exception Diverge
  
  fun fix f =
    let val r = ref (Except Diverge)
    in r := compute (fn _ => f r); force r end
end
```

This structure indicates that a suspension is internally represented as a mutable cell, whose internal state is one of the following:

* `Pure x`, if the suspension was already forced, and its final result is `x`.
* `Except e`, if the suspension was already forced, and an exception was thrown in the process.
* `Delay f`, if the suspension wasn't forced yet, and its final result can be obtained by evaluating `f ()`.

Furthermore, because we used *opaque ascription* (`:>`), the internal representation of the type of suspensions is hidden outside of the module.

---

Here's our new type of lazy suspensions in action:

```
infixr 5 :::
datatype 'a stream = NIL | ::: of 'a * 'a stream Lazy.lazy

(* An infinite stream of 1s, as in the Haskell example above *)
val xs = Lazy.fix (fn xs => 1 ::: xs)

(* Haskell's Data.List.unfoldr *)
fun unfoldr f x =
  case f x of
      NONE => NIL
    | SOME (x, y) => x ::: Lazy.delay (unfoldr f) y

(* Haskell's Prelude.iterate *)
fun iterate f x = x ::: Lazy.delay (iterate f o f) x

(* Two dummy suspensions *)
val foo = Lazy.pure 0
val bar = Lazy.pure 1

(* Illegal, foo and bar have type `int Lazy.lazy`,
 * whose internal representation as a mutable cell is hidden *)
val _ = (foo := !bar)
```

