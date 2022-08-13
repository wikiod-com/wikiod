---
title: "Java - Working with JSON"
slug: "java---working-with-json"
draft: false
images: []
weight: 9994
type: docs
toc: true
---



Play documentation: https://www.playframework.com/documentation/2.5.x/JavaJsonActions

## Manual creating JSON
<!-- language: lang-java -->
    import play.libs.Json;


    public JsonNode createJson() {
        // {"id": 33, "values": [3, 4, 5]}
        ObjectNode rootNode = Json.newObject();
        ArrayNode listNode = Json.newArray();

        long values[] = {3, 4, 5};
        for (long val: values) {
            listNode.add(val);
        }
        
        rootNode.put("id", 33);
        rootNode.set("values", listNode);
        return rootNode;
    }

## Loading json from string/file
<!-- language: lang-java -->    
    import play.libs.Json;
    // (...)
    


## Loading a file from your public folder

<!-- language: lang-java -->
    // Note: "app" is an play.Application instance
    JsonNode node = Json.parse(app.resourceAsStream("public/myjson.json"));


## Load from a string

<!-- language: lang-java -->
    String myStr = "{\"name\": \"John Doe\"}";
    JsonNode node = Json.parse(myStr);
 

## Transversing a JSON document
In the following examples, `json` contains a JSON object with the following data:

<!-- language: lang-json -->
    [
      {
        "name": "John Doe",
        "work": {
          "company": {
            "name": "ASDF INC",
            "country": "USA"
          },
          "cargo": "Programmer"
        },
        "tags": ["java", "jvm", "play"]
      },
      {
        "name": "Bob Doe",
        "work": {
          "company": {
            "name": "NOPE INC",
            "country": "AUSTRALIA"
          },
          "cargo": "SysAdmin"
        },
        "tags": ["puppet", "ssh", "networking"],
        "active": true
      }
    ]

## Get the name of some user (unsafe)

<!-- language: lang-java -->
    JsonNode node = json.get(0).get("name"); // --> "John Doe"
    // This will throw a NullPointerException, because there is only two elements
    JsonNode node = json.get(2).get("name"); // --> *crash*

## Get the user name (safe way)

<!-- language: lang-java -->
    JsonNode node1 = json.at("/0/name"); // --> TextNode("John Doe")
    JsonNode node2 = json.at("/2/name"); // --> MissingNode instance
    if (! node2.isMissingNode()) {
        String name = node2.asText();    
    }
    
## Get the country where first user works

<!-- language: lang-java -->
    JsonNode node2 = json.at("/0/work/company/country"); // TextNode("USA")
    

## Get every countries

<!-- language: lang-java -->
    List<JsonNode> d = json.findValues("country"); // List(TextNode("USA"), TextNode("AUSTRALIA"))
    
## Find every user that contains the attribute "active"

<!-- language: lang-java -->
    List<JsonNode> e = json.findParents("active"); // List(ObjectNode("Bob Doe"))

## Conversion between JSON and Java objects (basic)
By default, Jackson (the library Play JSON uses) will try to map every public field to a json field with the same name. If the object has getters/setters, it will infer the name from them. So, if you have a `Book` class with a private field to store the ISBN and have get/set methods named `getISBN/setISBN`, Jackson will

* Create a JSON object with the field "ISBN" when converting from Java to JSON
* Use the `setISBN` method to define the isbn field in the Java object (if the JSON object has a "ISBN" field).





## Create Java object from JSON

<!-- language: java -->
    public class Person {
        String id, name;
    }
    
    JsonNode node = Json.parse("{\"id\": \"3S2F\", \"name\", \"Salem\"}");
    Person person = Json.fromJson(node, Person.class);
    System.out.println("Hi " + person.name); // Hi Salem
    
## Create JSON object from Java object

<!-- language: java -->
    // "person" is the object from the previous example
    JsonNode personNode = Json.toJson(person) 
    
    
## Creating a JSON string from a JSON object

<!-- language: java -->
    // personNode comes from the previous example
    String json = personNode.toString();
    // or 
    String json = Json.stringify(json);
    

## JSON pretty printing

<!-- language: java -->
    System.out.println(personNode.toString());
    /* Prints:
    {"id":"3S2F","name":"Salem"}
    */   
    
    System.out.println(Json.prettyPrint(personNode));
    /* Prints:
    {
      "id" : "3S2F",
      "name" : "Salem"
    }
    */

