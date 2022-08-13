---
title: "Type Inference"
slug: "type-inference"
draft: false
images: []
weight: 9946
type: docs
toc: true
---

## Local Type Inference
Scala has a powerful type-inference mechanism built-in to the language. This mechanism is termed as 'Local Type Inference':

    val i = 1 + 2                  // the type of i is Int
    val s = "I am a String"        // the type of s is String
    def squared(x : Int) = x*x     // the return type of squared is Int
       
The compiler can infer the type of variables from the initialization expression. Similarly, the return type of methods can be omitted, since they are equivalent to the type returned by the method body. The above examples are equivalent to the below, explicit type declarations:

    val i: Int = 1 + 2               
    val s: String = "I am a String" 
    def squared(x : Int): Int = x*x     

## Type Inference And Generics
The Scala compiler can also deduce type parameters when polymorphic methods are called, or when generic classes are instantiated:

    case class InferedPair[A, B](a: A, b: B)

    val pairFirstInst = InferedPair("Husband", "Wife")  //type is InferedPair[String, String]
        
    // Equivalent, with type explicitly defined
    val pairSecondInst: InferedPair[String, String] 
                          = InferedPair[String, String]("Husband", "Wife")  
       
The above form of type inference is similar to the [Diamond Operator][1], introduced in Java 7.


  [1]: http://www.javaworld.com/article/2074080/core-java/jdk-7--the-diamond-operator.html

## Limitations to Inference
There are scenarios in which Scala type-inference does not work. For instance, the compiler cannot infer the type of method parameters:

    def add(a, b) = a + b  // Does not compile
    def add(a: Int, b: Int) = a + b // Compiles
    def add(a: Int, b: Int): Int = a + b // Equivalent expression, compiles

The compiler cannot infer the return type of recursive methods:

    // Does not compile
    def factorial(n: Int) = if (n == 0 || n == 1) 1 else n * factorial(n - 1)
    // Compiles
    def factorial(n: Int): Int = if (n == 0 || n == 1) 1 else n * factorial(n - 1)


## Preventing inferring Nothing
Based on [this blog post](http://www.tikalk.com/java/avoiding-nothing/).

Assume you have a method like this:

      def get[T]: Option[T] = ???

When you try to call it without specifying the generic parameter, `Nothing` gets inferred, which is not very useful for an actual implementation (and its result is not useful). With the following solution the `NotNothing` context bound can prevent using the method without specifying the expected type (in this example `RuntimeClass` is also excluded as for `ClassTags` not `Nothing`, but `RuntimeClass` is inferred):

    @implicitNotFound("Nothing was inferred")
    sealed trait NotNothing[-T]

    object NotNothing {
      implicit object notNothing extends NotNothing[Any]
      //We do not want Nothing to be inferred, so make an ambigous implicit
      implicit object `\n The error is because the type parameter was resolved to Nothing` extends NotNothing[Nothing]
      //For classtags, RuntimeClass can also be inferred, so making that ambigous too
      implicit object `\n The error is because the type parameter was resolved to RuntimeClass` extends NotNothing[RuntimeClass]
    }

    object ObjectStore {
      //Using context bounds
      def get[T: NotNothing]: Option[T] = {
        ???
      }
      
      def newArray[T](length: Int = 10)(implicit ct: ClassTag[T], evNotNothing: NotNothing[T]): Option[Array[T]] = ???
    }

Example usage:

    object X {
      //Fails to compile
      //val nothingInferred = ObjectStore.get
      
      val anOption = ObjectStore.get[String]
      val optionalArray = ObjectStore.newArray[AnyRef]()
      
      //Fails to compile
      //val runtimeClassInferred = ObjectStore.newArray()
    }

