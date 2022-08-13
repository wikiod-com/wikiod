---
title: "JSON"
slug: "json"
draft: false
images: []
weight: 9830
type: docs
toc: true
---

## JSON with Circe
[Circe][1] provides compile-time derived codecs for en/decode json into case classes. A simple example looks like this:

    import io.circe._
    import io.circe.generic.auto._
    import io.circe.parser._
    import io.circe.syntax._

    case class User(id: Long, name: String)
   
    val user = User(1, "John Doe")
    
    // {"id":1,"name":"John Doe"}
    val json = user.asJson.noSpaces

    // Right(User(1L, "John Doe"))
    val res: Either[Error, User] = decode[User](json)

  [1]: https://github.com/travisbrown/circe

## JSON with spray-json
[spray-json](https://github.com/spray/spray-json) provides an easy way to work with JSON. Using implicit formats, everything happens "behind the scenes":

# Make the Library Available with SBT

To manage `spray-json` with [SBT managed library dependencies][1]: 

    libraryDependencies += "io.spray" %% "spray-json" % "1.3.2"

Note that the last parameter, the version number (`1.3.2`), may be different in different projects.

The `spray-json` library is hosted at [repo.spray.io][2].

## Import the Library

    import spray.json._
    import DefaultJsonProtocol._

The default JSON protocol `DefaultJsonProtocol` contains formats for all basic types. To provide JSON functionality for custom types, either use convenience builders for formats or write formats explicitly.

# Read JSON

    // generates an intermediate JSON representation (abstract syntax tree)
    val res = """{ "foo": "bar" }""".parseJson // JsValue = {"foo":"bar"}

    res.convertTo[Map[String, String]] // Map(foo -> bar)

# Write JSON

    val values = List("a", "b", "c")
    values.toJson.prettyPrint // ["a", "b", "c"]

# DSL

DSL is not supported.

# Read-Write to Case Classes

The following example shows how to serialize a case class object into the JSON format.

    case class Address(street: String, city: String)
    case class Person(name: String, address: Address)

    // create the formats and provide them implicitly
    implicit val addressFormat = jsonFormat2(Address)
    implicit val personFormat = jsonFormat2(Person)

    // serialize a Person
    Person("Fred", Address("Awesome Street 9", "SuperCity"))
    val fredJsonString = fred.toJson.prettyPrint

This results in the following JSON:

    {
      "name": "Fred",
      "address": {
        "street": "Awesome Street 9",
        "city": "SuperCity"
      }
    }

That JSON can, in turn, be deserialized back into an object:

    val personRead = fredJsonString.parseJson.convertTo[Person] 
    //Person(Fred,Address(Awesome Street 9,SuperCity))

# Custom Format

Write a [custom `JsonFormat`][3] if a special serialization of a type is required. For example, if the field names are different in Scala than in JSON. Or, if  different concrete types are instantiated based on the input.

    implicit object BetterPersonFormat extends JsonFormat[Person] {
        // deserialization code
        override def read(json: JsValue): Person = {
            val fields = json.asJsObject("Person object expected").fields
            Person(
                name = fields("name").convertTo[String],
                address = fields("home").convertTo[Address]
            )
        }

        // serialization code
        override def write(person: Person): JsValue = JsObject(
            "name" -> person.name.toJson,
            "home" -> person.address.toJson
        )
    }


  [1]: https://www.wikiod.com/sbt/dependencies
  [2]: http://repo.spray.io/
  [3]: https://github.com/spray/spray-json#providing-jsonformats-for-other-types

## JSON with json4s
json4s uses implicit formats as other json frameworks.

SBT dependency: 
```
libraryDependencies += "org.json4s" %% "json4s-native" % "3.4.0"
//or
libraryDependencies += "org.json4s" %% "json4s-jackson" % "3.4.0"
```
**Imports**
```
import org.json4s.JsonDSL._
import org.json4s._
import org.json4s.native.JsonMethods._

implicit val formats = DefaultFormats
```
`DefaultFormats` contains default formats to read/write all basic types.

**Read json**
```
// generates an intermediate JSON representation (abstract syntax tree)
val res = parse("""{ "foo": "bar" }""")           // JValue = {"foo":"bar"}
res.extract[Map[String, String]]                  // Map(foo -> bar)
```

**Write json**
```
val values = List("a", "b", "c")
compact(render(values))         // ["a", "b", "c"]
```
**DSL**

```
json \ "foo"       //Simple path: JArray(List(JObject(List((foo,JString(bar))))))
json \\ "foo"      //Recursive path: ~List([{"foo":"bar"}], "bar")
(json \ "foo")(0)  //Index lookup (for JsArrays): JObject(List((foo,JString(bar))))
("foo" -> "bar") ~ ("field" -> "value")    // {"foo":"bar","field":"value"}
```
**Read and write to case class**
```
import org.json4s.native.Serialization.{read, write}

case class Address(street: String, city: String)
val addressString = write(Address("Awesome stree", "Super city")) 
// {"street":"Awesome stree","city":"Super city"}

read[Address](addressString) // Address(Awesome stree,Super city)
//or
parse(addressString).extract[Address]
```
**Read and Write heterogenous lists**

To serialize and deserialize an heterogenous (or polymorphic) list, specific type-hints need to be provided.
```
trait Location
case class Street(name: String) extends Location
case class City(name: String, zipcode: String) extends Location
case class Address(street: Street, city: City) extends Location
case class Locations (locations : List[Location])

implicit val formats = Serialization.formats(ShortTypeHints(List(classOf[Street], classOf[City], classOf[Address])))

val locationsString = write(Locations(Street("Lavelle Street"):: City("Super city","74658")))

read[Locations](locationsString)
```
**Own Format**
```
class AddressSerializer extends CustomSerializer[Address](format => (
  {
    case JObject(JField("Street", JString(s)) :: JField("City", JString(c)) :: Nil) =>
      new Address(s, c)
  },
  {
    case x: Address => ("Street" -> x.street) ~ ("City" -> x.city)
  }
  ))

implicit val formats = DefaultFormats + new AddressSerializer
val str = write[Address](Address("Awesome Stree", "Super City"))
// {"Street":"Awesome Stree","City":"Super City"}
read[Address](str) 
// Address(Awesome Stree,Super City)
```

## JSON with play-json

play-json uses implicit formats as other json frameworks

SBT dependency: `libraryDependencies += ""com.typesafe.play" %% "play-json" % "2.4.8"`
```
import play.api.libs.json._
import play.api.libs.functional.syntax._ // if you need DSL
```
`DefaultFormat` contains defaul formats to read/write all basic types. To provide JSON functionality for your own types, you can either use convenience builders for formats or write formats explicitly.

**Read json**

    // generates an intermediate JSON representation (abstract syntax tree)
    val res = Json.parse("""{ "foo": "bar" }""") // JsValue = {"foo":"bar"}
    
    res.as[Map[String, String]]                  // Map(foo -> bar)
    res.validate[Map[String, String]]            //JsSuccess(Map(foo -> bar),)
    
**Write json**
    
    val values = List("a", "b", "c")
    Json.stringify(Json.toJson(values))          // ["a", "b", "c"]

**DSL**

    val json = parse("""{ "foo": [{"foo": "bar"}]}""")
    (json \ "foo").get                    //Simple path: [{"foo":"bar"}]
    (json \\ "foo")                       //Recursive path:List([{"foo":"bar"}], "bar")
    (json \ "foo")(0).get                  //Index lookup (for JsArrays): {"foo":"bar"}

*As always prefer pattern matching against `JsSuccess`/`JsError` and try to avoid `.get`, `array(i)` calls.*

**Read and write to case class**

    case class Address(street: String, city: String)
    case class Person(name: String, address: Address)
    
    // create the formats and provide them implicitly
    implicit val addressFormat = Json.format[Address]
    implicit val personFormat = Json.format[Person]
    
    // serialize a Person
    val fred = Person("Fred", Address("Awesome Street 9", "SuperCity"))
    val fredJsonString = Json.stringify(Json.toJson(Json.toJson(fred)))
    
    val personRead = Json.parse(fredJsonString).as[Person] //Person(Fred,Address(Awesome Street 9,SuperCity))

**Own Format**

You can write your own JsonFormat if you require a special serialization of your type (e.g. name the fields differently in scala and Json or instantiate different concrete types based on the input)

    case class Address(street: String, city: String)
    
    // create the formats and provide them implicitly
    implicit object AddressFormatCustom extends Format[Address] {
      def reads(json: JsValue): JsResult[Address] = for {
        street <- (json \ "Street").validate[String]
        city <- (json \ "City").validate[String]
      } yield Address(street, city)
    
      def writes(x: Address): JsValue = Json.obj(
        "Street" -> x.street,
        "City" -> x.city
      )
    }
    // serialize an address
    val address = Address("Awesome Street 9", "SuperCity")
    val addressJsonString = Json.stringify(Json.toJson(Json.toJson(address)))
    //{"Street":"Awesome Street 9","City":"SuperCity"}
    
    val addressRead = Json.parse(addressJsonString).as[Address] 
    //Address(Awesome Street 9,SuperCity)

**Alternative**

If the json doesn't exactly match your case class fields (`isAlive` in case class vs `is_alive` in json):

    case class User(username: String, friends: Int, enemies: Int, isAlive: Boolean)
    
    object User {
    
      import play.api.libs.functional.syntax._
      import play.api.libs.json._
    
      implicit val userReads: Reads[User] = (
          (JsPath \ "username").read[String] and
          (JsPath \ "friends").read[Int] and
          (JsPath \ "enemies").read[Int] and
          (JsPath \ "is_alive").read[Boolean]
        ) (User.apply _)
    }

**Json with optional fields**

    case class User(username: String, friends: Int, enemies: Int, isAlive: Option[Boolean])
    
    object User {
    
      import play.api.libs.functional.syntax._
      import play.api.libs.json._
    
      implicit val userReads: Reads[User] = (
          (JsPath \ "username").read[String] and
          (JsPath \ "friends").read[Int] and
          (JsPath \ "enemies").read[Int] and
          (JsPath \ "is_alive").readNullable[Boolean]
        ) (User.apply _)
    }

**Reading timestamps from json**

Imagine you have a Json object, with a Unix timestamp field:


    {
      "field": "example field",
      "date": 1459014762000
    }

solution:

    case class JsonExampleV1(field: String, date: DateTime)
    object JsonExampleV1{
      implicit val r: Reads[JsonExampleV1] = (
        (__ \ "field").read[String] and
          (__ \ "date").read[DateTime](Reads.DefaultJodaDateReads)
        )(JsonExampleV1.apply _)
    }

**Reading custom case classes**

Now, if you do wrap your object identifiers for type safety, you will enjoy this. See the following json object:

    {
      "id": 91,
      "data": "Some data"
    }

and the corresponding case classes:

    case class MyIdentifier(id: Long)
    
    case class JsonExampleV2(id: MyIdentifier, data: String)

Now you just need to read the primitive type (Long), and map to your idenfier:

    object JsonExampleV2 {
      implicit val r: Reads[JsonExampleV2] = (
          (__ \ "id").read[Long].map(MyIdentifier) and
        (__ \ "data").read[String]
        )(JsonExampleV2.apply _)
    }

code at https://github.com/pedrorijo91/scala-play-json-examples

