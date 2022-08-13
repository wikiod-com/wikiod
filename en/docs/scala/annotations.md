---
title: "Annotations"
slug: "annotations"
draft: false
images: []
weight: 9989
type: docs
toc: true
---

## Syntax
 - @AnAnnotation def someMethod = {...}
 - @AnAnnotation class someClass {...}
 - @AnnotatioWithArgs(annotation_args) def someMethod = {...}


## Parameters
| Parameter | Details |
| --------- | ------- |
| @ | Indicates that the token following is an annotation. |
| SomeAnnotation | The name of the annotation |
| constructor_args | (optional) The arguments passed to the annotation. If none, the parentheses are unneeded. |

Scala-lang provides a [list of standard annotations and their Java equivalents][1].


  [1]: http://docs.scala-lang.org/tutorials/tour/annotations.html

## Using an Annotation
This sample annotation indicates that the following method is [`deprecated`][1].

    @deprecated
    def anUnusedLegacyMethod(someArg: Any) =  {
      ...
    }

This can also be equivalently written as:

    @deprecated def anUnusedLegacyMethod(someArg: Any) =  {
      ...
    }


  [1]: https://docs.oracle.com/javase/8/docs/api/java/lang/Deprecated.html

## Annotating the main constructor
    /**
     * @param num Numerator
     * @param denom Denominator
     * @throws ArithmeticException in case `denom` is `0`
     */
    class Division @throws[ArithmeticException](/*no annotation parameters*/) protected (num: Int, denom: Int) {
        private[this] val wrongValue = num / denom
        
        /** Integer number
         *  @param num Value */
        protected[Division] def this(num: Int) {
          this(num, 1)
        }
    }
    object Division {
      def apply(num: Int) = new Division(num)
      def apply(num: Int, denom: Int) = new Division(num, denom)
    }

The visibility modifier (in this case `protected`) should come after the annotations in the same line. In case the annotation accepts optional parameters (as in this case `@throws` accepts an optional cause), you have to specify an empty parameter list for the annotation: `()` before the constructor parameters.

Note: Multiple annotations can be specified, even from the same type ([repeating annotations](https://docs.oracle.com/javase/tutorial/java/annotations/repeating.html)).

Similarly with a case class without auxiliary factory method (and cause specified for the annotation):

    case class Division @throws[ArithmeticException]("denom is 0") (num: Int, denom: Int) {
        private[this] val wrongValue = num / denom
    }

## Creating Your Own Annotations
You can create you own Scala annotations by creating classes derived from scala.annotation.StaticAnnotation or scala.annotation.ClassfileAnnotation

    package animals
    // Create Annotation `Mammal`
    class Mammal(indigenous:String) extends scala.annotation.StaticAnnotation
    
    // Annotate class Platypus as a `Mammal`
    @Mammal(indigenous = "North America")
    class Platypus{}

Annotations can then be interrogated using the reflection API.
    
    scala>import scala.reflect.runtime.{universe ⇒ u}
    
    scala>val platypusType = u.typeOf[Platypus]
    platypusType: reflect.runtime.universe.Type = animals.reflection.Platypus

    scala>val platypusSymbol = platypusType.typeSymbol.asClass
    platypusSymbol: reflect.runtime.universe.ClassSymbol = class Platypus

    scala>platypusSymbol.annotations
    List[reflect.runtime.universe.Annotation] = List(animals.reflection.Mammal("North America"))


