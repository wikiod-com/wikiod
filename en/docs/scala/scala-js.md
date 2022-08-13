---
title: "Scala.js"
slug: "scalajs"
draft: false
images: []
weight: 9988
type: docs
toc: true
---

[`Scala.js`](http://www.scala-js.org/) is a port from `Scala` that compiles to `JavaScript`, which at the end will be running outside the `JVM`. It has benefits as strong typing, code optimization at compile time, full interoperability with JavaScript libraries.

## console.log in Scala.js
    println("Hello Scala.js") // In ES6: console.log("Hello Scala.js");

## Fat arrow functions
    val lastNames = people.map(p => p.lastName)
    // Or shorter:
    val lastNames = people.map(_.lastName)

## Simple Class
    class Person(val firstName: String, val lastName: String) {
      def fullName(): String =
        s"$firstName $lastName"
    }

## Collections
    val personMap = Map(
      10 -> new Person("Roger", "Moore"),
      20 -> new Person("James", "Bond")
    )
    val names = for {
      (key, person) <- personMap
      if key > 15
    } yield s"$key = ${person.firstName}"

## Manipulating DOM
    import org.scalajs.dom
    import dom.document
    
    def appendP(target: dom.Node, text: String) = {
      val pNode = document.createElement("p")
      val textNode = document.createTextNode(text)
      pNode.appendChild(textNode)
      target.appendChild(pNode)
    }

## Using with SBT
## Sbt dependency
    libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "0.9.1" // (Triple %%%)
## Running
    sbt run
## Running with continous compilation:
    sbt ~run
## Compile to a single JavaScript file:
    sbt fastOptJS


