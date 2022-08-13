---
title: "Macros"
slug: "macros"
draft: false
images: []
weight: 9979
type: docs
toc: true
---

Macros are a form of compile time metaprogramming. Certain elements of Scala code, such as annotations and methods, can be made to transform other code when they are compiled. Macros are ordinary Scala code that operate on data types that represent other code. The [Macro Paradise][] plugin extends the abilities of macros beyond the base language.

[Macro Paradise]: http://docs.scala-lang.org/overviews/macros/paradise.html

## Syntax
- def x() = macro x_impl // x is a macro, where x_impl is used to transform code
- def macroTransform(annottees: Any*): Any = macro impl // Use in annotations to make them macros

Macros are a language feature that need to be enabled, either by importing `scala.language.macros` or with the compiler option `-language:macros`. Only macro definitions require this; code that uses macros need not do it.

## Method Macros
When a method is defined to be a macro, the compiler takes the code that is passed as its argument and turns it into an AST. It then invokes the macro implementation with that AST, and it returns a new AST that is then spliced back to its call site.

    import reflect.macros.blackbox.Context

    object Macros {
      // This macro simply sees if the argument is the result of an addition expression.
      // E.g. isAddition(1+1) and isAddition("a"+1).
      // but !isAddition(1+1-1), as the addition is underneath a subtraction, and also
      // !isAddition(x.+), and !isAddition(x.+(a,b)) as there must be exactly one argument.
      def isAddition(x: Any): Boolean = macro isAddition_impl

      // The signature of the macro implementation is the same as the macro definition,
      // but with a new Context parameter, and everything else is wrapped in an Expr.
      def isAddition_impl(c: Context)(expr: c.Expr[Any]): c.Expr[Boolean] = {
        import c.universe._ // The universe contains all the useful methods and types
        val plusName = TermName("+").encodedName // Take the name + and encode it as $plus
        expr.tree match { // Turn expr into an AST representing the code in isAddition(...)
          case Apply(Select(_, `plusName`), List(_)) => reify(true)
          // Pattern match the AST to see whether we have an addition
          // Above we match this AST
          //             Apply (function application)
          //            /     \
          //         Select  List(_) (exactly one argument)
          // (selection ^ of entity, basically the . in x.y)
          //      /          \
          //    _              \
          //               `plusName` (method named +)
          case _                                     => reify(false)
          // reify is a macro you use when writing macros
          // It takes the code given as its argument and creates an Expr out of it
        }
      }
    }

It is also possible to have macros that take `Tree`s as arguments. Like how `reify` is used to create `Expr`s, the `q` (for quasiquote) string interpolator lets us create and deconstruct `Tree`s. Note that we could have used `q` above (`expr.tree` is, surprise, a `Tree` itself) too, but didn't for demonstrative purposes.
    
    // No Exprs, just Trees
    def isAddition_impl(c: Context)(tree: c.Tree): c.Tree = {
      import c.universe._
      tree match {
        // q is a macro too, so it must be used with string literals.
        // It can destructure and create Trees.
        // Note how there was no need to encode + this time, as q is smart enough to do it itself.
        case q"${_} + ${_}" => q"true"
        case _              => q"false"
      }
    }

## Macro Annotation
This simple macro annotation outputs the annotated item as-is.

```
import scala.annotation.{compileTimeOnly, StaticAnnotation}
import scala.reflect.macros.whitebox.Context

@compileTimeOnly("enable macro paradise to expand macro annotations")
class noop extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro linkMacro.impl
}

object linkMacro {
  def impl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._

    c.Expr[Any](q"{..$annottees}")
  }
}
```

The `@compileTimeOnly` annotation generates an error with a message indicating that the [`paradise` compiler plugin][1] must be included to use this macro. Instructions to include this [via SBT are here][2].

You can use the above-defined macro like this:
```
@noop
case class Foo(a: String, b: Int)

@noop
object Bar {
  def f(): String = "hello"
}

@noop
def g(): Int = 10
```


  [1]: http://docs.scala-lang.org/overviews/macros/paradise.html
  [2]: https://www.wikiod.com/sbt/projects#Configure Macros in a Project

## Errors in Macros
Macros can trigger compiler warnings and errors through the use of their `Context`.

Say we're a particularly overzealous when it comes to bad code, and we want to mark every instance of technical debt with a compiler info message (let's not think about how bad this idea is). We can use a macro that does nothing except emit such a message.

    import reflect.macros.blackbox.Context

    def debtMark(message: String): Unit = macro debtMark_impl
    def debtMarkImpl(c: Context)(message: c.Tree): c.Tree = {
      message match {
        case Literal(Constant(msg: String)) => c.info(c.enclosingPosition, msg, false)
        // false above means "do not force this message to be shown unless -verbose"
        case _                              => c.abort(c.enclosingPosition, "Message must be a string literal.")
        // Abort causes the compilation to completely fail. It's not even a compile error, where
        // multiple can stack up; this just kills everything.
      }
      q"()" // At runtime this method does nothing, so we return ()
    }

Additionally, instead of using `???` to mark unimplemented code, we can create two macros, `!!!` and `?!?`, that serve the same purpose, but emit compiler warnings. `?!?` will cause a warning to be issued, and `!!!` will cause an outright error.

    import reflect.macros.blackbox.Context

    def ?!? : Nothing = macro impl_?!?
    def !!! : Nothing = macro impl_!!!

    def impl_?!?(c: Context): c.Tree = {
      import c.universe._
      c.warning(c.enclosingPosition, "Unimplemented!")
      q"${termNames.ROOTPKG}.scala.Predef.???"
      // If someone were to shadow the scala package, scala.Predef.??? would not work, as it
      // would end up referring to the scala that shadows and not the actual scala.
      // ROOTPKG is the very root of the tree, and acts like it is imported anew in every
      // expression. It is actually named _root_, but if someone were to shadow it, every
      // reference to it would be an error. It allows us to safely access ??? and know that
      // it is the one we want.
    }

    def impl_!!!(c: Context): c.Tree = {
      import c.universe._
      c.error(c.enclosingPosition, "Unimplemented!")
      q"${termNames.ROOTPKG}.scala.Predef.???"
    }

