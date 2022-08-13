---
title: "Working with JSON - Scala"
slug: "working-with-json---scala"
draft: false
images: []
weight: 9985
type: docs
toc: true
---

[Official documentation][1]
[Package documentation][2]


You can use the play json package independently from Play by including 

`"com.typesafe.play" % "play-json_2.11" % "2.5.3"` in your `build.sbt`, see
* https://mvnrepository.com/artifact/com.typesafe.play/play-json_2.11
* http://stackoverflow.com/questions/19436069/adding-play-json-library-to-sbt


  [1]: https://www.playframework.com/documentation/2.5.x/ScalaJson
  [2]: https://www.playframework.com/documentation/2.5.x/api/scala/index.html#play.api.libs.package

## Creating a JSON manually
<!-- language-all: lang-scala -->

You can build a JSON object tree (a `JsValue`) manually

    import play.api.libs.json._

    val json = JsObject(Map(
      "name" -> JsString("Jsony McJsonface"),
      "age" -> JsNumber(18),
      "hobbies" -> JsArray(Seq(
        JsString("Fishing"),
        JsString("Hunting"),
        JsString("Camping")
      ))
    ))

Or with the shorter equivalent syntax, based on a few implicit conversions :

    import play.api.libs.json._

    val json = Json.obj(
      "name" -> "Jsony McJsonface",
      "age" -> 18,
      "hobbies" -> Seq(
        "Fishing",
        "Hunting",
        "Camping"
      )
    )

To get the JSON string :

    json.toString
    // {"name":"Jsony McJsonface","age":18,"hobbies":["Fishing","Hunting","Camping"]}
    Json.prettyPrint(json)  
    //  {
    //    "name" : "Jsony McJsonface",
    //    "age" : 18,
    //    "hobbies" : [ "Fishing", "Hunting", "Camping" ]
    //  }

## Java: Accepting JSON requests


## Java: Accepting JSON requests with BodyParser


## Scala: Reading a JSON manually
<!-- language-all: lang-scala -->

If you are given a JSON string :

    val str =
        """{
        |    "name" : "Jsony McJsonface",
        |    "age" : 18,
        |    "hobbies" : [ "Fishing", "Hunting", "Camping" ],
        |    "pet" : {
        |        "name" : "Doggy",
        |        "type" : "dog"
        |    }
        |}""".stripMargin

You can parse it to get a JsValue, representing the JSON tree

    val json = Json.parse(str)

And traverse the tree to lookup specific values :

    (json \ "name").as[String]          // "Jsony McJsonface"

Useful methods
=====

 - `\` to go to a specific key in a JSON object
 - `\\` to go to all occurences of a specific key in a JSON object, searching recursively in nested objects
 - `.apply(idx)` (i.e. `(idx)`) to go to a index in an array
 - `.as[T]` to cast to a precise subtype
 - `.asOpt[T]` to attempt to cast to a precise subtype, returning None if it's the wrong type
 - `.validate[T]` to attempt to cast a JSON value to a precise subtype, returning a JsSuccess or a JsError


    (json \ "name").as[String]          // "Jsony McJsonface"
    (json \ "pet" \ "name").as[String]  // "Doggy"
    (json \\ "name").map(_.as[String])  // List("Jsony McJsonface", "Doggy")
    (json \\ "type")(0).as[String]      // "dog"
    (json \ "wrongkey").as[String]      // throws JsResultException
    (json \ "age").as[Int]              // 18
    (json \ "hobbies").as[Seq[String]]  // List("Fishing", "Hunting", "Camping")
    (json \ "hobbies")(2).as[String]    // "Camping"
    (json \ "age").asOpt[String]        // None
    (json \ "age").validate[String]     // JsError containing some error detail


## Mapping automatically to/from case classes
<!-- language-all: lang-scala -->

Overall the easiest way to work with JSON is to have a case class mapping directly to the JSON
(same fields name, equivalent types, etc.).

    case class Person(
      name: String,
      age: Int,
      hobbies: Seq[String],
      pet: Pet
    )

    case class Pet(
      name: String,
      `type`: String
    )

    // these macros will define automatically the conversion to/from JSON
    // based on the cases classes definition
    implicit val petFormat = Json.format[Pet]
    implicit val personFormat = Json.format[Person]

Converting to Json
==================

    val person = Person(
      "Jsony McJsonface",
      18,
      Seq("Fishing", "Hunting", "Camping"),
      Pet("Doggy", "dog")
    )

    Json.toJson(person).toString
    // {"name":"Jsony McJsonface","age":18,"hobbies":["Fishing","Hunting","Camping"],"pet":{"name":"Doggy","type":"dog"}}

Converting from Json
==================

    val str =
        """{
        |    "name" : "Jsony McJsonface",
        |    "age" : 18,
        |    "hobbies" : [ "Fishing", "Hunting", "Camping" ],
        |     "pet" : {
        |       "name" : "Doggy",
        |       "type" : "dog"
        |     }
        |}""".stripMargin


    Json.parse(str).as[Person]
    // Person(Jsony McJsonface,18,List(Fishing, Hunting, Camping),Pet(Doggy,dog))



