---
title: "Enumerations"
slug: "enumerations"
draft: false
images: []
weight: 9883
type: docs
toc: true
---

Approach with `sealed trait` and `case objects` is preferred because Scala enumeration has a few problems:
 
 1. Enumerations have the same type after erasure.
 2. Compiler doesn't complain about “Match is not exhaustive", if case is missed it will fail in runtime `scala.MatchError`:


    def isWeekendWithBug(day: WeekDays.Value): Boolean = day match {
      case WeekDays.Sun | WeekDays.Sat => true
    }
    
    isWeekendWithBug(WeekDays.Fri)
    scala.MatchError: Fri (of class scala.Enumeration$Val)

Compare with:

    def isWeekendWithBug(day: WeekDay): Boolean = day match {
      case WeekDay.Sun | WeekDay.Sat => true
    }

    Warning: match may not be exhaustive.
    It would fail on the following inputs: Fri, Mon, Thu, Tue, Wed
    def isWeekendWithBug(day: WeekDay): Boolean = day match {
                                              ^
More detailed explanation is presented in this [article about Scala Enumeration][1].


  [1]: http://underscore.io/blog/posts/2014/09/03/enumerations.html

## Using sealed trait and case objects
An alternative to extending `Enumeration` is using `sealed` case objects:

    sealed trait WeekDay
    
    object WeekDay {
      case object Mon extends WeekDay
      case object Tue extends WeekDay
      case object Wed extends WeekDay
      case object Thu extends WeekDay
      case object Fri extends WeekDay
      case object Sun extends WeekDay
      case object Sat extends WeekDay
    }

The `sealed` keyword guarantees that the trait `WeekDay` cannot be extended in another file. This allows the compiler to make certain assumptions, including that all possible values of `WeekDay` are already enumerated.

One drawback is that this method does not allow you to obtain a list of all possible values. To get such a list it must be provided explicitly:

    val allWeekDays = Seq(Mon, Tue, Wed, Thu, Fri, Sun, Sat)

Case classes can also extend a `sealed` trait. Thus, objects and case classes can be mixed to create complex hierarchies:

    sealed trait CelestialBody
        
    object CelestialBody {
      case object Earth extends CelestialBody
      case object Sun extends CelestialBody
      case object Moon extends CelestialBody
      case class Asteroid(name: String) extends CelestialBody
    }

Another drawback is that there is no way to access a the variable name of a `sealed` object's enumeration, or search by it. If you need some kind of name associated to each value, it must be manually defined:

      sealed trait WeekDay { val name: String }

      object WeekDay {
          case object Mon extends WeekDay { val name = "Monday" }
          case object Tue extends WeekDay { val name = "Tuesday" }
          (...)   
      }

Or just:
     
      sealed case class WeekDay(name: String)
        
      object WeekDay {
          object Mon extends WeekDay("Monday")
          object Tue extends WeekDay("Tuesday")
          (...)   
      }


## Days of the week using Scala Enumeration
Java-like enumerations can be created by extending [Enumeration][1].

    object WeekDays extends Enumeration {
      val Mon, Tue, Wed, Thu, Fri, Sat, Sun = Value
    }
    
    def isWeekend(day: WeekDays.Value): Boolean = day match {
      case WeekDays.Sat | WeekDays.Sun => true
      case _ => false
    }
    
    isWeekend(WeekDays.Sun)
    res0: Boolean = true


It is also possible to add a human-readable name for values in an enumeration:

    object WeekDays extends Enumeration {
          val Mon = Value("Monday")
          val Tue = Value("Tuesday")
          val Wed = Value("Wednesday")
          val Thu = Value("Thursday")
          val Fri = Value("Friday")
          val Sat = Value("Saturday")
          val Sun = Value("Sunday")
    }
    
    println(WeekDays.Mon)
    >> Monday

    WeekDays.withName("Monday") == WeekDays.Mon
    >> res0: Boolean = true

Beware of the not-so-typesafe behavior, wherein different enumerations can evaluate as the same instance type:

    object Parity extends Enumeration {
       val Even, Odd = Value
    }
      
    WeekDays.Mon.isInstanceOf[Parity.Value]
    >> res1: Boolean = true


  [1]: http://www.scala-lang.org/api/2.11.4/scala/Enumeration.html

## Using sealed trait and case objects and allValues-macro
This is just an extension on the sealed trait variant where a macro generates a set with all instances at compile time. 
This nicely omits the drawback that a developer can add a value to the enumeration but forget to add it to the allElements set.

This variant especially becomes handy for large enums.


    import EnumerationMacros._
    
    sealed trait WeekDay
    object WeekDay {
      case object Mon extends WeekDay
      case object Tue extends WeekDay
      case object Wed extends WeekDay
      case object Thu extends WeekDay
      case object Fri extends WeekDay
      case object Sun extends WeekDay
      case object Sat extends WeekDay
      val allWeekDays: Set[WeekDay] = sealedInstancesOf[WeekDay]
    }

For this to work you need this macro:

    import scala.collection.immutable.TreeSet
    import scala.language.experimental.macros
    import scala.reflect.macros.blackbox
    
    /**
    A macro to produce a TreeSet of all instances of a sealed trait.
    Based on Travis Brown's work:
    http://stackoverflow.com/questions/13671734/iteration-over-a-sealed-trait-in-scala
    CAREFUL: !!! MUST be used at END OF code block containing the instances !!!
    */
    object EnumerationMacros {
      def sealedInstancesOf[A]: TreeSet[A] = macro sealedInstancesOf_impl[A]
    
      def sealedInstancesOf_impl[A: c.WeakTypeTag](c: blackbox.Context) = {
        import c.universe._
    
        val symbol = weakTypeOf[A].typeSymbol.asClass
    
        if  (!symbol.isClass || !symbol.isSealed)
          c.abort(c.enclosingPosition, "Can only enumerate values of a sealed trait or class.")
        else {
    
          val children = symbol.knownDirectSubclasses.toList
    
          if (!children.forall(_.isModuleClass)) c.abort(c.enclosingPosition, "All children must be objects.")
          else c.Expr[TreeSet[A]] {
    
            def sourceModuleRef(sym: Symbol) = Ident(sym.asInstanceOf[scala.reflect.internal.Symbols#Symbol
              ].sourceModule.asInstanceOf[Symbol]
            )
    
            Apply(
              Select(
                reify(TreeSet).tree,
                TermName("apply")
              ),
              children.map(sourceModuleRef(_))
            )
          }
        }
      }
    }

