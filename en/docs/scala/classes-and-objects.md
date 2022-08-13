---
title: "Classes and Objects"
slug: "classes-and-objects"
draft: false
images: []
weight: 9951
type: docs
toc: true
---

## Syntax
 - `class MyClass{}  // curly braces are optional here as class body is empty`
 - `class MyClassWithMethod {def method: MyClass = ???}`
 - `new MyClass() //Instantiate`
 - `object MyObject // Singleton object`
 - `class MyClassWithGenericParameters[V1, V2](vl: V1, i: Int, v2: V2)`
 - `class MyClassWithImplicitFieldCreation[V1](val v1: V1, val i: Int)`
 - `new MyClassWithGenericParameters(2.3, 4, 5)` or with a different type: `new MyClassWithGenericParameters[Double, Any](2.3, 4, 5)`
 - `class MyClassWithProtectedConstructor protected[my.pack.age](s: String)`

## Singleton & Companion Objects
Singleton Objects
-----------------

Scala supports static members, but not in the same manner as Java. Scala provides an alternative to this called *Singleton Objects*. Singleton objects are similar to a normal class, except they can not be instantiated using the `new` keyword. Below is a sample singleton class:

    object Factorial {
        private val cache = Map[Int, Int]()
        def getCache = cache
    }

Note that we have used `object` keyword to define singleton object (instead of 'class' or 'trait'). Since singleton objects can not be instantiated they can not have parameters. Accessing a singleton object looks like this:

    Factorial.getCache() //returns the cache

Note that this looks exactly like accessing a static method in a Java class.

Companion Objects
-----------------

In Scala singleton objects may share the name of a corresponding class. In such a scenario the singleton object is referred to as a *Companion Object*. For instance, below the class `Factorial` is defined, and a companion object (also named `Factorial`) is defined below it. By convention companion objects are defined in the same file as their companion class.

    class Factorial(num : Int) {
    
      def fact(num : Int) : Int = if (num <= 1) 1 else (num * fact(num - 1))
    
      def calculate() : Int = {
        if (!Factorial.cache.contains(num)) {    // num does not exists in cache
          val output = fact(num) // calculate factorial
          Factorial.cache += (num -> output)     // add new value in cache
        }
    
        Factorial.cache(num)
      }
    }
    
    object Factorial {
      private val cache = scala.collection.mutable.Map[Int, Int]()
    }

    val factfive = new Factorial(5)
    factfive.calculate  // Calculates the factorial of 5 and stores it
    factfive.calculate  // uses cache this time
    val factfiveagain = new Factorial(5)
    factfiveagain.calculate  // Also uses cache

In this example we are using a private `cache` to store factorial of a number to save calculation time for repeated numbers.

Here `object Factorial` is a companion object and `class Factorial` is its corresponding companion class. Companion objects and classes can access each other's `private` members. In the example above `Factorial` class is accessing the private `cache` member of it's companion object.

Note that a new instantiation of the class will still utilize the same companion object, so any modification to member variables of that object will carry over.

## Constructors
## Primary Constructor ##

In Scala the primary constructor is the body of the class. The class name is followed by a parameter list, which are the constructor arguments. (As with any function, an empty parameter list may be omitted.)

    class Foo(x: Int, y: String) {
        val xy: String = y * x
        /* now xy is a public member of the class */
    }

    class Bar {
        ...
    }

The construction parameters of an instance are not accessible outside its constructor body unless marked as an instance member by the `val` keyword:

    class Baz(val z: String) 
    // Baz has no other members or methods, so the body may be omitted

    val foo = new Foo(4, "ab")
    val baz = new Baz("I am a baz")
    foo.x // will not compile: x is not a member of Foo
    foo.xy // returns "abababab": xy is a member of Foo
    baz.z // returns "I am a baz": z is a member of Baz
    val bar0 = new Bar
    val bar1 = new Bar() // Constructor parentheses are optional here

Any operations that should be performed when an instance of an object is instantiated are written directly in the body of the class:

    class DatabaseConnection
        (host: String, port: Int, username: String, password: String) {
        /* first connect to the DB, or throw an exception */
        private val driver = new AwesomeDB.Driver()
        driver.connect(host, port, username, password)
        def isConnected: Boolean = driver.isConnected
        ...
    }

Note that it is considered good practice to put as few side effects into the constructor as possible; instead of the above code, one should consider having `connect` and `disconnect` methods so that consumer code is responsible for scheduling IO.

## Auxiliary Constructors ##

A class may have additional constructors called 'auxiliary constructers'. These are defined by constructor definitions in the form `def this(...) = e`, where `e` must invoke another constructor:

    class Person(val fullName: String) {    
      def this(firstName: String, lastName: String) = this(s"$firstName $lastName")
    }

    // usage:
    new Person("Grace Hopper").fullName // returns Grace Hopper
    new Person("Grace", "Hopper").fullName // returns Grace Hopper

This implies each constructor can have a different modifier: only some may be available publicly:

    class Person private(val fullName: String) {    
      def this(firstName: String, lastName: String) = this(s"$firstName $lastName")
    }

    new Person("Ada Lovelace") // won't compile
    new Person("Ada", "Lovelace") // compiles

In this way you can control how consumer code may instantiate the class.

## Instantiate Class Instances
A class in Scala is a 'blueprint' of a class instance. An instance contains the state and behavior as defined by that class. To declare a class:

    class MyClass{}  // curly braces are optional here as class body is empty

An instance can be instantiated using `new` keyword:

    var instance = new MyClass()

or:

    var instance = new MyClass

Parentheses are optional in Scala for creating objects from a class that has a no-argument constructor. If a class constructor takes arguments:

    class MyClass(arg : Int)       // Class definition
    var instance = new MyClass(2)  // Instance instantiation
    instance.arg                   // not allowed

Here `MyClass` requires one `Int` argument, which can only be used internally to the class. `arg` cannot be accessed outside `MyClass` unless it is declared as a field:

    class MyClass(arg : Int){ 
        val prop = arg  // Class field declaration
    } 

    var obj = new MyClass(2)
    obj.prop     // legal statement

Alternatively it can be declared public in the constructor:

    class MyClass(val arg : Int)   // Class definition with arg declared public
    var instance = new MyClass(2)  // Instance instantiation
    instance.arg                   //arg is now visible to clients

## Instantiating class with no parameter: {} vs ()
Let's say we have a class MyClass with no constructor argument:

    class MyClass

In Scala we can instantiate it using below syntax:

    val obj = new MyClass()

Or we can simply write:

    val obj = new MyClass

But, if not paid attention, in some cases optional parenthesis may produce some unexpected behavior. Suppose we want to create a task that should run in a separate thread. Below is the sample code:

    val newThread = new Thread { new Runnable {
            override def run(): Unit = {
                // perform task
                println("Performing task.")
            }
          }
        }

    newThread.start   // prints no output

We may think that this sample code if executed will print `Performing task.`, but to our surprise, it won't print anything. Let's see what's happening here. If you pay a closer look, we have used curly braces `{}`, right after `new Thread`. It created an annonymous class which extends `Thread`:

    val newThread = new Thread {
      //creating anonymous class extending Thread
    }

And then in the body of this annonymous class, we defined our task (again creating an annonymous class implementing `Runnable` interface). So we might have thought that we used `public Thread(Runnable target)` constructor but in fact (by ignoring optional `()`) we used `public Thread()` constructor with nothing defined in the body of `run()` method. To rectify the problem, we need to use parenthesis instead of curly braces.

    val newThread = new Thread ( new Runnable {
            override def run(): Unit = {
                // perform task
                println("Performing task.")
            }
          }
        )

In other words, here `{}` and `()` are not *interchangeable*.

## Objects
Whereas Classes are more like blueprints, Objects are static (i.e. already instantiated):

    object Dog {
        def bark: String = "Raf"
    }

    Dog.bark() // yields "Raf"

They are often used as a companion to a class, they allow you to write:
    
    class Dog(val name: String) {
    
    }

    object Dog {
        def apply(name: String): Dog = new Dog(name)
    }

    val dog = Dog("Barky") // Object
    val dog = new Dog("Barky") // Class



## Instance type checking
**Type check**: `variable.isInstanceOf[Type]`

With [pattern matching](https://www.wikiod.com/scala/pattern-matching) (not so useful in this form): 

    variable match {
      case _: Type => true
      case _ => false
    }

Both `isInstanceOf` and pattern matching are checking only the object's type, not its generic parameter (no type reification), except for arrays:

    val list: List[Any] = List(1, 2, 3)             //> list  : List[Any] = List(1, 2, 3)
  
    val upcasting = list.isInstanceOf[Seq[Int]]     //> upcasting  : Boolean = true
  
    val shouldBeFalse = list.isInstanceOf[List[String]]
                                                    //> shouldBeFalse  : Boolean = true

But

    val chSeqArray: Array[CharSequence] = Array("a") //> chSeqArray  : Array[CharSequence] = Array(a)
    val correctlyReified = chSeqArray.isInstanceOf[Array[String]]
                                                  //> correctlyReified  : Boolean = false


    val stringIsACharSequence: CharSequence = ""    //> stringIsACharSequence  : CharSequence = ""
      
    val sArray = Array("a")                         //> sArray  : Array[String] = Array(a)
    val correctlyReified = sArray.isInstanceOf[Array[String]]
                                                    //> correctlyReified  : Boolean = true
    
    //val arraysAreInvariantInScala: Array[CharSequence] = sArray
    //Error: type mismatch;  found   : Array[String]  required: Array[CharSequence]
    //Note: String <: CharSequence, but class Array is invariant in type T.
    //You may wish to investigate a wildcard type such as `_ <: CharSequence`. (SLS 3.2.10)
    //Workaround:
    val arraysAreInvariantInScala: Array[_ <: CharSequence] = sArray
                                                    //> arraysAreInvariantInScala  : Array[_ <: CharSequence] = Array(a)
      
    
    val arraysAreCovariantOnJVM = sArray.isInstanceOf[Array[CharSequence]]
                                                    //> arraysAreCovariantOnJVM  : Boolean = true

**Type casting**: `variable.asInstanceOf[Type]`

With [pattern matching](https://www.wikiod.com/scala/pattern-matching):

    variable match {
      case _: Type => true
    }

Examples:

      val x = 3                                       //> x  : Int = 3
      x match {
        case _: Int => true//better: do something
        case _ => false
      }                                               //> res0: Boolean = true
      
      x match {
        case _: java.lang.Integer => true//better: do something
        case _ => false
      }                                               //> res1: Boolean = true
      
      x.isInstanceOf[Int]                             //> res2: Boolean = true
      
      //x.isInstanceOf[java.lang.Integer]//fruitless type test: a value of type Int cannot also be a Integer
      
      trait Valuable { def value: Int}
      case class V(val value: Int) extends Valuable
      
      val y: Valuable = V(3)                          //> y  : Valuable = V(3)
      y.isInstanceOf[V]                               //> res3: Boolean = true
      y.asInstanceOf[V]                               //> res4: V = V(3)


Remark: This is only about the behaviour on the JVM, on other platforms (JS, native) type casting/checking might behave differently.



