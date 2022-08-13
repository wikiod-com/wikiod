---
title: "Dynamic Invocation"
slug: "dynamic-invocation"
draft: false
images: []
weight: 9989
type: docs
toc: true
---

Scala allows you to use dynamic invocation when calling methods or accessing fields on an object. Instead of having this built deep into the language, this is accomplished through rewriting rules similar to those of implicit conversions, enabled by the marker trait [`scala.Dynamic`][Dynamic scaladoc]. This allows you to emulate the ability to dynamically add properties to objects present in dynamic languages, and more.

[Dynamic scaladoc]: http://www.scala-lang.org/api/2.12.x/scala/Dynamic.html

## Syntax
- class Foo extends Dynamic
- foo.field
- foo.field = value
- foo.method(args)
- foo.method(namedArg = x, y)

In order to declare subtypes of `Dynamic`, the language feature `dynamics` must be enabled, either by importing `scala.language.dynamics` or by the `-language:dynamics` compiler option. Users of this `Dynamic` who do not define their own subtypes do not need to enable this.

## Field Accesses
This:

    class Foo extends Dynamic {
      // Expressions are only rewritten to use Dynamic if they are not already valid
      // Therefore foo.realField will not use select/updateDynamic
      var realField: Int = 5
      // Called for expressions of the type foo.field
      def selectDynamic(fieldName: String) = ???
      def updateDynamic(fieldName: String)(value: Int) = ???
    }

allows for simple access to fields:

    val foo: Foo = ???
    foo.realField // Does NOT use Dynamic; accesses the actual field
    foo.realField = 10 // Actual field access here too
    foo.unrealField // Becomes foo.selectDynamic(unrealField)
    foo.field = 10  // Becomes foo.updateDynamic("field")(10)
    foo.field = "10" // Does not compile; "10" is not an Int.
    foo.x() // Does not compile; Foo does not define applyDynamic, which is used for methods.
    foo.x.apply() // DOES compile, as Nothing is a subtype of () => Any
    // Remember, the compiler is still doing static type checks, it just has one more way to
    // "recover" and rewrite otherwise invalid code now.

## Method Calls
This:

    class Villain(val minions: Map[String, Minion]) extends Dynamic {
      def applyDynamic(name: String)(jobs: Task*) = jobs.foreach(minions(name).do)
      def applyDynamicNamed(name: String)(jobs: (String, Task)*) = jobs.foreach {
        // If a parameter does not have a name, and is simply given, the name passed as ""
        case ("", task) => minions(name).do(task)
        case (subsys, task) => minions(name).subsystems(subsys).do(task)
      }
    }

allows for calls to methods, with and without named parameters:

    val gru: Villain = ???
    gru.blu() // Becomes gru.applyDynamic("blu")()
    // Becomes gru.applyDynamicNamed("stu")(("fooer", ???), ("boomer", ???), ("", ???),
    //         ("computer breaker", ???), ("fooer", ???))
    // Note how the `???` without a name is given the name ""
    // Note how both occurrences of `fooer` are passed to the method
    gru.stu(fooer = ???, boomer = ???, ???, `computer breaker` = ???, fooer = ???)
    gru.ERR("a") // Somehow, scalac thinks "a" is not a Task, though it clearly is (it isn't)

## Interaction Between Field Access and Update Method
Slightly counterintuitively (but also the only sane way to make it work), this:

    val dyn: Dynamic = ???
    dyn.x(y) = z

is equivalent to:

    dyn.selectDynamic("x").update(y, z)

while

    dyn.x(y)

is still

    dyn.applyDynamic("x")(y)

It is important to be aware of this, or else it may sneak by unnoticed and cause strange errors.

