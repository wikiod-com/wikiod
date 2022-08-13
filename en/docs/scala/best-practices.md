---
title: "Best Practices"
slug: "best-practices"
draft: false
images: []
weight: 9969
type: docs
toc: true
---

> Prefer vals, immutable objects, and methods without side effects.
> Reach for them first. Use vars, mutable objects, and methods with side
> effects when you have a specific need and justification for them.

 -- *Programming in Scala*, by Odersky, Spoon, and Venners


There are more example and guideline in [this presentation][1] by Odersky.


  [1]: http://www.slideshare.net/Typesafe/scaladays-keynote

## Keep it simple
Do not overcomplicate simple tasks. Most of the time you will need only:
 - algebraic datatypes 
 - structural recursion 
 - monad-like api (`map`, `flatMap`, `fold`)

There is plenty of complicated stuff in Scala, such as:
 - `Cake pattern` or `Reader Monad` for Dependency Injection.
 - Passing arbitrary values as `implicit` arguments.

These things are not clear for newcomers: avoid using them before you understand them. Using advanced concepts without a real need obfuscates the code, making it *less* maintainable.

## Don't pack too much in one expression.
- Find meaningful names for computation units. 
- Use `for` comprehensions or `map` to combine computations together.

Let's say you have something like this:
```
if (userAuthorized.nonEmtpy) {
  makeRequest().map {
    case Success(respone) =>
      someProcessing(..)
      if (resendToUser) {
        sendToUser(...)
      }
    ...
  }
}
```

If all your functions return `Either` or another `Validation`-like type, you can write:
```
for {
  user     <- authorizeUser
  response <- requestToThirdParty(user)
  _        <- someProcessing(...)
} {
  sendToUser
}
```

## Prefer a Functional Style, Reasonably
By default:
- Use `val`, not `var`, wherever possible. This allows you to take seamless advantage of a number of functional utilities, including work distribution.
- Use [`recursion`][1] and [`comprehensions`][2]s, not loops. 
- Use immutable collections. This is a corrolary to using `val` whenever possible.
- Focus on data transformations, CQRS-style logic, and not CRUD.

There are good reasons to choose non-functional style:
- `var` can be used for local state (for example, inside an actor).
- `mutable` gives better performance in certain situations.


  [1]: https://www.wikiod.com/scala/recursion
  [2]: https://www.wikiod.com/scala/for-expressions

