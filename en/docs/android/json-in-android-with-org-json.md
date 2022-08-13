---
title: "JSON in Android with org.json"
slug: "json-in-android-with-orgjson"
draft: false
images: []
weight: 9086
type: docs
toc: true
---

## Syntax
 - **Object** : An object is an unordered set of name/value pairs. An object begins with { (left brace) and ends with } (right brace). Each name is followed by : (colon) and the name/value pairs are separated by , (comma).

 - **Array** : An array is an ordered collection of values. An array begins with [ (left bracket) and ends with ] (right bracket). Values are separated by , (comma).

 - **Value** : A value can be a string in double quotes, or a number, or true or false or null, or an object or an array. These structures can be nested.

 - **String** : A string is a sequence of zero or more Unicode characters, wrapped in double quotes, using backslash escapes. A character is represented as a single character string. A string is very much like a C or Java string.

 - **Number** : A number is very much like a C or Java number, except that the octal and hexadecimal formats are not used.

This topic is about using the [`org.json`][1] package that is included in the Android SDK.


  [1]: https://developer.android.com/reference/org/json/package-summary.html

## Creating a simple JSON object
Create the [`JSONObject`][1] using the empty constructor and add fields using the [`put()`][2] method, which is overloaded so that it can be used with different types:

    try {
        // Create a new instance of a JSONObject
        final JSONObject object = new JSONObject();
        
        // With put you can add a name/value pair to the JSONObject
        object.put("name", "test");
        object.put("content", "Hello World!!!1");
        object.put("year", 2016);
        object.put("value", 3.23);
        object.put("member", true);
        object.put("null_value", JSONObject.NULL);

        // Calling toString() on the JSONObject returns the JSON in string format.
        final String json = object.toString();
        
    } catch (JSONException e) {
        Log.e(TAG, "Failed to create JSONObject", e);
    }


The resulting `JSON` string looks like this: 

    {  
       "name":"test",
       "content":"Hello World!!!1",
       "year":2016,
       "value":3.23,
       "member":true,
       "null_value":null
    }


  [1]: https://developer.android.com/reference/org/json/JSONObject.html
  [2]: https://developer.android.com/reference/org/json/JSONObject.html#put(java.lang.String,%20double)

## Create a JSON String with null value.
If you need to produce a JSON string with a value of `null` like this:

    {  
       "name":null
    }

Then you have to use the special constant [JSONObject.NULL][1].


 **Functioning example:**

    jsonObject.put("name", JSONObject.NULL);


  [1]: https://developer.android.com/reference/org/json/JSONObject.html#NULL

## Add JSONArray to JSONObject
    // Create a new instance of a JSONArray
    JSONArray array = new JSONArray();

    // With put() you can add a value to the array.
    array.put("ASDF");
    array.put("QWERTY");

    // Create a new instance of a JSONObject
    JSONObject obj = new JSONObject();

    try {
        // Add the JSONArray to the JSONObject
        obj.put("the_array", array);
    } catch (JSONException e) {
        e.printStackTrace();
    }
    
    String json = obj.toString();

The resulting JSON string looks like this:

    {  
       "the_array":[  
          "ASDF",
          "QWERTY"
       ]
    }



## Parse simple JSON object
Consider the following JSON string:

    {
      "title": "test",
      "content": "Hello World!!!",
      "year": 2016,
      "names" : [
            "Hannah",
            "David",
            "Steve"
       ]
    } 

This JSON object can be parsed using the following code:

    try {
        // create a new instance from a string
        JSONObject jsonObject = new JSONObject(jsonAsString);
        String title = jsonObject.getString("title");
        String content = jsonObject.getString("content");
        int year = jsonObject.getInt("year");
        JSONArray names = jsonObject.getJSONArray("names"); //for an array of String objects
    } catch (JSONException e) {
        Log.w(TAG,"Could not parse JSON. Error: " + e.getMessage());
    }


**Here is another example with a JSONArray nested inside JSONObject:**

    {
        "books":[
          {
            "title":"Android JSON Parsing",
            "times_sold":186
          }
        ]
    }

This can be parsed with the following code:

    JSONObject root = new JSONObject(booksJson);
    JSONArray booksArray = root.getJSONArray("books");
    JSONObject firstBook = booksArray.getJSONObject(0);
    String title = firstBook.getString("title");
    int timesSold = firstBook.getInt("times_sold");




## Create nested JSON object
To produce nested JSON object, you need to simply add one JSON object to another:

    JSONObject mainObject = new JSONObject();            // Host object
    JSONObject requestObject = new JSONObject();         // Included object

    try {
        requestObject.put("lastname", lastname);
        requestObject.put("phone", phone);
        requestObject.put("latitude", lat);
        requestObject.put("longitude", lon);
        requestObject.put("theme", theme);
        requestObject.put("text", message);

        mainObject.put("claim", requestObject);
    } catch (JSONException e) {
        return "JSON Error";
    }

Now `mainObject` contains a key called `claim` with the whole `requestObject` as a value. 

## Check for the existence of fields on JSON
Sometimes it's useful to check if a field is present or absent on your JSON to avoid some `JSONException` on your code. 

To achieve that, use the [`JSONObject#has(String)`][1] or the  method, like on the following example:

**Sample JSON**

    {
       "name":"James"
    }

**Java code**

    String jsonStr = " { \"name\":\"James\" }";
    JSONObject json = new JSONObject(jsonStr);
    // Check if the field "name" is present
    String name, surname;
    
    // This will be true, since the field "name" is present on our JSON.
    if (json.has("name")) {
        name = json.getString("name");
    }
    else {
        name = "John";
    }
    // This will be false, since our JSON doesn't have the field "surname".
    if (json.has("surname")) {
        surname = json.getString("surname");
    }
    else {
        surname = "Doe";
    }
    
    // Here name == "James" and surname == "Doe".


  [1]: https://developer.android.com/reference/org/json/JSONObject.html#has(java.lang.String)

## Working with null-string when parsing json
    {
        "some_string": null,
        "ather_string": "something"
    }

If we will use this way:

    JSONObject json = new JSONObject(jsonStr);
    String someString = json.optString("some_string");

We will have output:

    someString = "null";

So we need to provide this workaround:


    /**
     * According to http://stackoverflow.com/questions/18226288/json-jsonobject-optstring-returns-string-null
     * we need to provide a workaround to opt string from json that can be null.
     * <strong></strong>
     */
    public static String optNullableString(JSONObject jsonObject, String key) {
        return optNullableString(jsonObject, key, "");
    }

    /**
     * According to http://stackoverflow.com/questions/18226288/json-jsonobject-optstring-returns-string-null
     * we need to provide a workaround to opt string from json that can be null.
     * <strong></strong>
     */
    public static String optNullableString(JSONObject jsonObject, String key, String fallback) {
        if (jsonObject.isNull(key)) {
            return fallback;
        } else {
            return jsonObject.optString(key, fallback);
        }
    }

And then call:

    JSONObject json = new JSONObject(jsonStr);
    String someString = optNullableString(json, "some_string");
    String someString2 = optNullableString(json, "some_string", "");

And we will have Output as we expected:

    someString = null; //not "null"
    someString2 = "";


## Using JsonReader to read JSON from a stream
`JsonReader` reads a JSON encoded value as a stream of tokens.

       public List<Message> readJsonStream(InputStream in) throws IOException {
         JsonReader reader = new JsonReader(new InputStreamReader(in, "UTF-8"));
         try {
           return readMessagesArray(reader);
         } finally {
           reader.close();
         }
       }
    
       public List<Message> readMessagesArray(JsonReader reader) throws IOException {
         List<Message> messages = new ArrayList<Message>();
    
         reader.beginArray();
         while (reader.hasNext()) {
           messages.add(readMessage(reader));
         }
         reader.endArray();
         return messages;
       }
    
       public Message readMessage(JsonReader reader) throws IOException {
         long id = -1;
         String text = null;
         User user = null;
         List<Double> geo = null;
    
         reader.beginObject();
         while (reader.hasNext()) {
           String name = reader.nextName();
           if (name.equals("id")) {
             id = reader.nextLong();
           } else if (name.equals("text")) {
             text = reader.nextString();
           } else if (name.equals("geo") && reader.peek() != JsonToken.NULL) {
             geo = readDoublesArray(reader);
           } else if (name.equals("user")) {
             user = readUser(reader);
           } else {
             reader.skipValue();
           }
         }
         reader.endObject();
         return new Message(id, text, user, geo);
       }
    
       public List<Double> readDoublesArray(JsonReader reader) throws IOException {
         List<Double> doubles = new ArrayList<Double>();
    
         reader.beginArray();
         while (reader.hasNext()) {
           doubles.add(reader.nextDouble());
         }
         reader.endArray();
         return doubles;
       }
    
       public User readUser(JsonReader reader) throws IOException {
         String username = null;
         int followersCount = -1;
    
         reader.beginObject();
         while (reader.hasNext()) {
           String name = reader.nextName();
           if (name.equals("name")) {
             username = reader.nextString();
           } else if (name.equals("followers_count")) {
             followersCount = reader.nextInt();
           } else {
             reader.skipValue();
           }
         }
         reader.endObject();
         return new User(username, followersCount);
       }

## Handling dynamic key for JSON response
This is an example for how to handle dynamic key for response. Here `A` and `B` are dynamic keys it can be anything

Response

    {
      "response": [
        {
          "A": [
            {
              "name": "Tango"
            },
            {
              "name": "Ping"
            }
          ],
          "B": [
            {
              "name": "Jon"
            },
            {
              "name": "Mark"
            }
          ]
        }
      ]
    }

Java code

    // ResponseData is raw string of response
    JSONObject responseDataObj = new JSONObject(responseData);
    JSONArray responseArray = responseDataObj.getJSONArray("response");
    for (int i = 0; i < responseArray.length(); i++) {
        // Nodes ArrayList<ArrayList<String>> declared globally
        nodes = new ArrayList<ArrayList<String>>();
        JSONObject obj = responseArray.getJSONObject(i);
        Iterator keys = obj.keys();
        while(keys.hasNext()) {
           // Loop to get the dynamic key
           String currentDynamicKey = (String)keys.next();
           // Get the value of the dynamic key
           JSONArray currentDynamicValue = obj.getJSONArray(currentDynamicKey);
           int jsonArraySize = currentDynamicValue.length();
           if(jsonArraySize > 0) {
               for (int ii = 0; ii < jsonArraySize; ii++) {
                    // NameList ArrayList<String> declared globally
                    nameList = new ArrayList<String>();
                   if(ii == 0) {
                    JSONObject nameObj = currentDynamicValue.getJSONObject(ii);
                    String name = nameObj.getString("name");
                    System.out.print("Name = " + name);
                    // Store name in an array list
                    nameList.add(name);
                  }
               }                    
           }
         nodes.add(nameList);
        }
    }

## Updating the elements in the JSON

sample json to update

     {
     "student":{"name":"Rahul", "lastname":"sharma"},
     "marks":{"maths":"88"}
     }

To update the elements value in the json we need to assign the value and update.

    try {
        // Create a new instance of a JSONObject
        final JSONObject object = new JSONObject(jsonString);
        
        JSONObject studentJSON = object.getJSONObject("student");
        studentJSON.put("name","Kumar");
      
         object.remove("student");

         object.put("student",studentJSON);

        // Calling toString() on the JSONObject returns the JSON in string format.
        final String json = object.toString();
        
    } catch (JSONException e) {
        Log.e(TAG, "Failed to create JSONObject", e);
    }

updated value

     {
     "student":{"name":"Kumar", "lastname":"sharma"},
     "marks":{"maths":"88"}
     }



