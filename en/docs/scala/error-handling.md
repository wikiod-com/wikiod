---
title: "Error Handling"
slug: "error-handling"
draft: false
images: []
weight: 9938
type: docs
toc: true
---

## Either
Different data types for error/success

    def getPersonFromWebService(url: String): Either[String, Person] = {
        
        val response = webServiceClient.get(url)

        response.webService.status match {
            case 200 => {
                val person = parsePerson(response)
                if(!isValid(person)) Left("Validation failed")
                else Right(person)
            }

            case _ => Left(s"Request failed with error code $response.status")
        }
    }

Pattern matching on Either value

    getPersonFromWebService("http://some-webservice.com/person") match {
        case Left(errorMessage) => println(errorMessage)
        case Right(person) => println(person.surname)
    }

Convert Either value to Option

    val maybePerson: Option[Person] = getPersonFromWebService("http://some-webservice.com/person").right.toOption

## Try
Using Try with `map`, `getOrElse` and `flatMap`:

    import scala.util.Try

    val i = Try("123".toInt)     // Success(123)
    i.map(_ + 1).getOrElse(321)  // 124
    
    val j = Try("abc".toInt)     // Failure(java.lang.NumberFormatException)
    j.map(_ + 1).getOrElse(321)  // 321

    Try("123".toInt) flatMap { i =>
      Try("234".toInt)
        .map(_ + i)
    }                            // Success(357)

Using `Try` with pattern matching:

    Try(parsePerson("John Doe")) match {
        case Success(person) => println(person.surname)
        case Failure(ex) => // Handle error ...
    }

## Option
The use of `null` values is strongly discouraged, unless interacting with legacy Java code that expects `null`. Instead, [`Option`](https://www.wikiod.com/scala/option-class) should be used when the result of a function might either be something (`Some`) or nothing (`None`).

A try-catch block is more appropriate for error-handling, but if the function might legitimately return nothing, `Option` is appropriate to use, and simple. 

An `Option[T]` can either be `Some(value)` (contains a value of type `T`) or `None`:

    def findPerson(name: String): Option[Person]

If no person is found, `None` can be returned. Otherwise, an object of type `Some` containing a `Person` object is returned. What follows are ways to handle an object of type `Option`.

## Pattern Matching

    findPerson(personName) match {
        case Some(person) => println(person.surname)
        case None => println(s"No person found with name $personName")
    }

## Using **map** and **getOrElse**

    val name = findPerson(personName).map(_.firstName).getOrElse("Unknown")
    println(name) // Prints either the name of the found person or "Unknown"


## Using **fold**

    val name = findPerson(personName).fold("Unknown")(_.firstName)
    // equivalent to the map getOrElse example above.

## Converting to Java

If you need to convert an `Option` type to a null-able Java type for interoperability:

```scala
val s: Option[String] = Option("hello")
s.orNull           // "hello": String
s.getOrElse(null)  // "hello": String

val n: Option[Int] = Option(42)
n.orNull           // compilation failure (Cannot prove that Null <:< Int.)
n.getOrElse(null)  // 42
```

## Handling Errors Originating in Futures
When an `exception` is thrown from within a `Future`, you can (should) use `recover` to handle it.

For instance,

    def runFuture: Future = Future { throw new FairlyStupidException }

    val itWillBeAwesome: Future = runFuture

...will throw an `Exception` from within the `Future`. But seeing as we can predict that an `Exception` of type `FairlyStupidException` with a high probability, we can specifically handle this case in an elegant way:

    val itWillBeAwesomeOrIllRecover = runFuture recover { 
        case stupid: FairlyStupidException => 
             BadRequest("Another stupid exception!") 
    }

As you can see the method given to `recover` is a `PartialFunction` over the domain of all `Throwable`, so you can handle just a certain few types and then let the rest go into the ether of exception handling at higher levels in the `Future` stack.

Note that this is similar to running the following code in a non-`Future` context:

    def runNotFuture: Unit = throw new FairlyStupidException
    
    try {
        runNotFuture
    } catch {
        case e: FairlyStupidException => BadRequest("Another stupid exception!")
    }

It is really important to handle exceptions generated within `Future`s because   much of the time they are more insidious. They don't get all in your face usually, because they run in a different execution context and thread, and thus do not prompt you to fix them when they happen, especially if you don't notice anything in logs or the behavior of the application.

## Using try-catch clauses
In addition to functional constructs such as `Try`, `Option` and `Either` for error handling, Scala also supports a syntax similar to Java's, using a try-catch clause (with a potential finally block as well). The catch clause is a pattern match:

    try { 
      // ... might throw exception
    } catch {
      case ioe: IOException => ... // more specific cases first
      case e: Exception => ...
      // uncaught types will be thrown
    } finally {
      // ...
    }


## Convert Exceptions into Either or Option Types
To convert exceptions into `Either` or `Option` types, you can use methods that provided in `scala.util.control.Exception`

```
import scala.util.control.Exception._

val plain = "71a"
val optionInt: Option[Int] = catching(classOf[java.lang.NumberFormatException]) opt { plain.toInt }
val eitherInt = Either[Throwable, Int] = catching(classOf[java.lang.NumberFormatException]) either { plain.toInt }
```

