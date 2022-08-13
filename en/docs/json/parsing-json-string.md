---
title: "Parsing JSON string"
slug: "parsing-json-string"
draft: false
images: []
weight: 9933
type: docs
toc: true
---

## Parse JSON string in JavaScript
In JavaScript, the `JSON` object is used to parse a JSON string. This method is only available in modern browsers (IE8+, Firefox 3.5+, etc).

When a valid JSON string is parsed, the result is a JavaScript object, array or other value.

    JSON.parse('"bar of foo"')
    // "bar of foo" (type string)
    JSON.parse("true")
    // true (type boolean)
    JSON.parse("1")
    // 1 (type number)
    JSON.parse("[1,2,3]")
    // [1, 2, 3] (type array)
    JSON.parse('{"foo":"bar"}')
    // {foo: "bar"} (type object)
    JSON.parse("null")
    // null (type object)

Invalid strings will throw a JavaScript error

    JSON.parse('{foo:"bar"}')
    // Uncaught SyntaxError: Unexpected token f in JSON at position 1
    JSON.parse("[1,2,3,]")
    // Uncaught SyntaxError: Unexpected token ] in JSON at position 7
    JSON.parse("undefined")
    // Uncaught SyntaxError: Unexpected token u in JSON at position 0

The `JSON.parse` method includes an optional reviver function which can limit or modify the parse result

    JSON.parse("[1,2,3,4,5,6]", function(key, value) {
      return value > 3 ? '' : value;
    })
    // [1, 2, 3, "", "", ""]

    var x = {};
    JSON.parse('{"a":1,"b":2,"c":3,"d":4,"e":5,"f":6}', function(key, value) {
      if (value > 3) { x[key] = value; }
    })
    // x = {d: 4, e: 5, f: 6}

In the last example, the `JSON.parse` returns an `undefined` value. To prevent that, return the `value` inside the reviver function.

## Parse JSON string using com.google.gson library in Java
`com.google.gson` library needs to be added to use this code.

**Here is the example string:** 

    String companyDetails = {"companyName":"abcd","address":"abcdefg"}

**JSON strings can be parsed using below syntax in Java:**
 
    JsonParser parser =  new JsonParser();
    JsonElement jsonElement = parser.parse(companyDetails);
    JsonObject jsonObj = jsonElement.getAsJsonObject();
    String comapnyName = jsonObj.get("companyName").getAsString();

## Parse JSON file with Groovy
Suppose we have the following JSON data:

    {
        "TESTS": 
        [
             {
                  "YEAR": "2017",
                  "MONTH": "June",
                  "DATE": "28"                  
             }
        ]
    }

import groovy.json.JsonSlurper

class JSONUtils {
    
    private def data;
    private def fileName = System.getProperty("jsonFileName")
    
    public static void main(String[] args)
    {
        JSONUtils jutils = new JSONUtils()
        def month = jutils.get("MONTH");
    }    
    
Below is the parser:

    private parseJSON(String fileName = "data.json")
    { 
        def jsonSlurper = new JsonSlurper()
        def reader

        if(this.fileName?.trim())
        {
            fileName = this.fileName
        }

        reader = new BufferedReader(new InputStreamReader(new FileInputStream(fileName),"UTF-8"));
        data = jsonSlurper.parse(reader);
        return data
    }

    def get(String item)
    {
        def result = new ArrayList<String>();
        data = parseJSON()        
        data.TESTS.each{result.add(it."${item}")}
        return  result
    }        
}

