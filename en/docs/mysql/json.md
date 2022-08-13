---
title: "JSON"
slug: "json"
draft: false
images: []
weight: 9686
type: docs
toc: true
---

As of MySQL 5.7.8, MySQL supports a native JSON data type that enables efficient access to data in JSON (JavaScript Object Notation) documents.

https://dev.mysql.com/doc/refman/5.7/en/json.html

Starting from MySQL 5.7.8, MySQL ships with a JSON type. Lots of devs have been saving JSON data in text columns for a log time but the JSON type is different, the data is saved in binary format after validation. That avoids the overhead of parsing the text on each read.

## Create simple table with a primary key and JSON field
    CREATE TABLE table_name (
        id INT NOT NULL AUTO_INCREMENT, 
        json_col JSON,
        PRIMARY KEY(id)
    );



## Insert a simple JSON
    INSERT INTO
        table_name (json_col) 
    VALUES
        ('{"City": "Galle", "Description": "Best damn city in the world"}');

That's simple as it can get but note that because JSON dictionary keys have to be surrounded by double quotes the entire thing should be wrapped in single quotes. If the query succeeds, the data will be stored in a binary format.

## Updating a JSON field
In the previous example we saw how mixed data types can be inserted into a JSON field. What if we want to update that field? We are going to add *scheveningen* to the array named `variations` in the previous example.

    UPDATE 
        myjson 
    SET 
        dict=JSON_ARRAY_APPEND(dict,'$.variations','scheveningen') 
    WHERE 
        id = 2;

Notes:  
1) The `$.variations` array in our json dictionary. The $ symbol represents the json documentation. For a full explaination of json paths recognized by mysql refer to https://dev.mysql.com/doc/refman/5.7/en/json-path-syntax.html  
2) Since we don't yet have an example on querying using json fields, this example uses the primary key.

Now if we do `SELECT * FROM myjson` we will see

    +----+-----------------------------------------------------------------------------------------+
    | id | dict                                                                                    |
    +---+-----------------------------------------------------------------------------------------+
    | 2  | {"opening": "Sicilian", "variations": ["pelikan", "dragon", "najdorf", "scheveningen"]} |
    +----+-----------------------------------------------------------------------------------------+
    1 row in set (0.00 sec)


 


## Insert mixed data into a JSON field.
This inserts a json dictionary where one of the members is an array of strings into the table that was created in another example.

    INSERT INTO myjson(dict) 
    VALUES('{"opening":"Sicilian","variations":["pelikan","dragon","najdorf"]}');

Note, once again, that you need to be careful with the use of single and double quotes. The whole thing has to be wrapped in single quotes.



## CAST data to JSON type
This converts valid json strings to MySQL JSON type:

     SELECT CAST('[1,2,3]' as JSON) ;
     SELECT CAST('{"opening":"Sicilian","variations":["pelikan","dragon","najdorf"]}' as JSON);

## Create Json Object and Array
`JSON_OBJECT` creates JSON Objects:

     SELECT JSON_OBJECT('key1',col1 , 'key2',col2 , 'key3','col3') as myobj;


`JSON_ARRAY`  creates JSON Array as well:

     SELECT JSON_ARRAY(col1,col2,'col3') as myarray;

Note: myobj.key3 and myarray[2] are "col3" as fixed string.

Also mixed JSON data:

     SELECT JSON_OBJECT("opening","Sicilian", "variations",JSON_ARRAY("pelikan","dragon","najdorf") ) as mymixed ;

