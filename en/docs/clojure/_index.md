---
title : clojure Tutorial
slug : clojure-tutorial
weight : 9466
draft : false
images : []
type : docs
---

[![enter image description here][1]][1]

[Clojure][2] **is a dynamically-typed general-purpose programming language with Lisp syntax.**

Its features support the functional style of programming with first-class functions and immutable values by default. Using reassignable variables is not as easy in Clojure as in many mainstream languages, since variables have to be created and updated like container objects. This encourages use of pure values that will stay the way they were at the moment they were last seen. This typically makes code much more predictable, testable and concurrency-capable. This works for collections too, since Clojure's built-in data structures are persistent.

For performance, Clojure supports type-hinting to eliminate unnecessary reflection where possible. Also, groups of changes to persistent collections can be done to *transient* versions, reducing the amount of objects involved. This is not necessary most of the time, since persistent collections fast to copy since they share most of their data. Their performance guarantees are not far from their mutable counterparts.

Among other features, Clojure also has:

* software transactional memory (STM)
* several concurrency primitives not involving manual locking (atom, agent)
* composable sequence transformers (transducers),
* functional tree manipulation facilities (zippers)

Due to its simple syntax and high extensibility (via macros, implementation of core interfaces and reflection), some commonly-seen language features can be added to Clojure with libraries. For instance, `core.typed` brings a static type checker, `core.async` brings simple channel-based concurrency mechanisms, `core.logic` brings logic programming.

Designed as a hosted language, it can interoperate with the platform it runs on. While the primary target is JVM and the entire ecosystem behind Java, alternative implementations can run in other environments too, such as ClojureCLR running on the Common Language Runtime or ClojureScript running on JavaScript runtimes (including web browsers). While alternative implementations may lack some of the functionality from the JVM version, they are still considered one family of languages.


  [1]: http://i.stack.imgur.com/wb9tC.png
  [2]: https://clojure.org/

