---
title: "JSON in Java"
slug: "json-in-java"
draft: false
images: []
weight: 9717
type: docs
toc: true
---

JSON (JavaScript Object Notation) is a lightweight, text-based, language-independent data exchange format that is easy for humans and machines to read and write. JSON can represent two structured types: objects and arrays. JSON is often used in Ajax applications, configurations, databases, and RESTful web services.
[The Java API for JSON Processing][1] provides portable APIs to parse, generate, transform, and query JSON.


  [1]: http://www.oracle.com/technetwork/articles/java/json-1973242.html

This example focuses on parsing and creating JSON in Java using various libraries such as the **[Google Gson](https://github.com/google/gson)** library, Jackson Object Mapper, and others..

Examples using other libraries could be found here: [How to parse JSON in Java](http://stackoverflow.com/q/2591098/5020253)

## Using Jackson Object Mapper
Pojo Model

    public class Model {
        private String firstName;
        private String lastName;
        private int age;
        /* Getters and setters not shown for brevity */        
    }

Example: String to Object

    Model outputObject = objectMapper.readValue(
         "{\"firstName\":\"John\",\"lastName\":\"Doe\",\"age\":23}",
         Model.class);
    System.out.println(outputObject.getFirstName());
    //result: John

Example: Object to String 

    String jsonString = objectMapper.writeValueAsString(inputObject));
    //result: {"firstName":"John","lastName":"Doe","age":23}

# Details

Import statement needed:

    import com.fasterxml.jackson.databind.ObjectMapper;

[Maven dependency: jackson-databind](https://mvnrepository.com/artifact/com.fasterxml.jackson.core/jackson-databind)

## `ObjectMapper` instance

    //creating one
    ObjectMapper objectMapper = new ObjectMapper();

* `ObjectMapper` is threadsafe
* recommended: have a shared, static instance


## Deserialization:
        
    <T> T readValue(String content, Class<T> valueType)  

 * `valueType` needs to be specified -- the return will be of this type
 * Throws 
   * `IOException` - in case of a low-level I/O problem 
   * `JsonParseException` - if underlying input contains invalid content
   * `JsonMappingException` - if the input JSON structure does not match object structure 

Usage example (jsonString is the input string):
    
    Model fromJson = objectMapper.readValue(jsonString, Model.class);

## Method for serialization:

   String writeValueAsString(Object value)  

 * Throws 
   * `JsonProcessingException` in case of an error
   * Note: prior to version 2.1, throws clause included IOException; 2.1 removed it.


## JSON To Object  (Gson Library)
Lets assume you have a class called `Person` with just `name`

    private class Person {
        public String name;
    
        public Person(String name) {
            this.name = name;
        }
    }

*Code:*

    Gson gson = new Gson();
    String json = "{\"name\": \"John\"}";

    Person person = gson.fromJson(json, Person.class);
    System.out.println(person.name); //John

You must have [gson library][1] in your classpath.


  [1]: https://mvnrepository.com/artifact/com.google.code.gson/gson

## JSONObject.NULL


## JSON Builder - chaining methods


## Object To JSON (Gson Library)
Lets assume you have a class called `Person` with just `name`

    private class Person {
        public String name;
    
        public Person(String name) {
            this.name = name;
        }
    }

*Code:*

    Gson g = new Gson();

    Person person = new Person("John");
    System.out.println(g.toJson(person)); // {"name":"John"}

Of course the [Gson][1] jar must be on the classpath.


  [1]: http://central.maven.org/maven2/com/google/code/gson/gson/2.3.1/gson-2.3.1.jar

## JSON Iteration


## optXXX vs getXXX methods


## Encoding data as JSON
If you need to create a `JSONObject` and put data in it, consider the following example:

    // Create a new javax.json.JSONObject instance.
    JSONObject first = new JSONObject();
    
    first.put("foo", "bar");
    first.put("temperature", 21.5);
    first.put("year", 2016);

    // Add a second object.
    JSONObject second = new JSONObject();
    second.put("Hello", "world");
    first.put("message", second);
    
    // Create a new JSONArray with some values
    JSONArray someMonths = new JSONArray(new String[] { "January", "February" });
    someMonths.put("March");
    // Add another month as the fifth element, leaving the 4th element unset.
    someMonths.put(4, "May");
    
    // Add the array to our object
    object.put("months", someMonths);
    
    // Encode
    String json = object.toString();

    // An exercise for the reader: Add pretty-printing!
    /* {
           "foo":"bar",
           "temperature":21.5,
           "year":2016,
           "message":{"Hello":"world"},
           "months":["January","February","March",null,"May"]
       }
    */

## Decoding JSON data
If you need to get data from a `JSONObject`, consider the following example: 

    String json = "{\"foo\":\"bar\",\"temperature\":21.5,\"year\":2016,\"message\":{\"Hello\":\"world\"},\"months\":[\"January\",\"February\",\"March\",null,\"May\"]}";
    
    // Decode the JSON-encoded string
    JSONObject object = new JSONObject(json);
    
    // Retrieve some values
    String foo = object.getString("foo");
    double temperature = object.getDouble("temperature");
    int year = object.getInt("year");

    // Retrieve another object
    JSONObject secondary = object.getJSONObject("message");
    String world = secondary.getString("Hello");
    
    // Retrieve an array
    JSONArray someMonths = object.getJSONArray("months");
    // Get some values from the array
    int nMonths = someMonths.length();
    String february = someMonths.getString(1);

## Extract single element from JSON
    String json = "{\"name\": \"John\", \"age\":21}";
    
    JsonObject jsonObject = new JsonParser().parse(json).getAsJsonObject();

    System.out.println(jsonObject.get("name").getAsString()); //John
    System.out.println(jsonObject.get("age").getAsInt()); //21

## JsonArray to Java List (Gson Library)
Here is a simple JsonArray which you would like to convert to a Java `ArrayList`:

    {
        "list": [
                    "Test_String_1",
                    "Test_String_2"
                ] 
    }

Now pass the `JsonArray` 'list' to the following method which returns a corresponding Java `ArrayList`:

    public ArrayList<String> getListString(String jsonList){
        Type listType = new TypeToken<List<String>>() {}.getType();
        //make sure the name 'list' matches the name of 'JsonArray' in your 'Json'.
        ArrayList<String> list = new Gson().fromJson(jsonList, listType);    
        return list;
    }

You should add the following maven dependency to your `POM.xml` file:

    <!-- https://mvnrepository.com/artifact/com.google.code.gson/gson -->
    <dependency>
        <groupId>com.google.code.gson</groupId>
        <artifactId>gson</artifactId>
        <version>2.7</version>
    </dependency>

Or you should have the jar `com.google.code.gson:gson:jar:<version>` in your classpath.


## Deserialize JSON collection to collection of Objects using Jackson
Suppose you have a pojo class `Person`

    public class Person {
        public String name;

        public Person(String name) {
            this.name = name;
        }
    }


And you want to parse it into a JSON array or a map of Person objects. Due to type erasure you cannot construct classes of `List<Person>` and `Map<String, Person>` at runtime directly _(and thus use them to deserialize JSON)_. To overcome this limitation jackson provides two approaches - `TypeFactory` and `TypeReference`.

**TypeFactory**

The approach taken here is to use a factory (and its static utility function) to build your type for you. The parameters it takes are the collection you want to use (list, set, etc.) and the class you want to store in that collection.

**TypeReference**

The type reference approach seems simpler because it saves you a bit of typing and looks cleaner. TypeReference accepts a type parameter, where you pass the desired type `List<Person>`. You simply instantiate this TypeReference object and use it as your type container.

Now let's look at how to actually deserialize your JSON into a Java object. If your JSON is formatted as an array, you can deserialize it as a List. If there is a more complex nested structure, you will want to deserialize to a Map. We will look at examples of both.

# Deserializing JSON array

    String jsonString = "[{\"name\": \"Alice\"}, {\"name\": \"Bob\"}]"

## TypeFactory approach

    CollectionType listType = 
        factory.constructCollectionType(List.class, Person.class);
    List<Preson> list = mapper.readValue(jsonString, listType);

## TypeReference approach

    TypeReference<Person> listType = new TypeReference<List<Person>>() {};
    List<Person> list = mapper.readValue(jsonString, listType);

# Deserializing JSON map

    String jsonString = "{\"0\": {\"name\": \"Alice\"}, \"1\": {\"name\": \"Bob\"}}"

## TypeFactory approach

    CollectionType mapType = 
        factory.constructMapLikeType(Map.class, String.class, Person.class);
    List<Person> list = mapper.readValue(jsonString, mapType);

## TypeReference approach

    TypeReference<Person> mapType = new TypeReference<Map<String, Person>>() {};
    Map<String, Person> list = mapper.readValue(jsonString, mapType);


# Details

Import statement used:

    import com.fasterxml.jackson.core.type.TypeReference;
    import com.fasterxml.jackson.databind.ObjectMapper;
    import com.fasterxml.jackson.databind.type.CollectionType;

Instances used:

    ObjectMapper mapper = new ObjectMapper();
    TypeFactory factory = mapper.getTypeFactory();

# Note

While `TypeReference` approach may look better it has several drawbacks:
 1. `TypeReference` should be instantiated using anonymous class 
 2. You should provide generic explicity

Failing to do so may lead to loss of generic type argument which will lead to deserialization failure.

