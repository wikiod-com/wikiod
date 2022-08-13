---
title: "JSON Support"
slug: "json-support"
draft: false
images: []
weight: 9909
type: docs
toc: true
---

JSON - Java Script Object Notation , Postgresql support JSON Data type since 9.2 version. There are some predefined function and operators to access the JSON data.

The `->` operator returns the key of JSON column.
The `->>` operator returns the value of JSON Column.

## Using JSONb operators
# Creating a DB and a Table

    DROP DATABASE IF EXISTS books_db;
    CREATE DATABASE books_db WITH ENCODING='UTF8' TEMPLATE template0;
    
    DROP TABLE IF EXISTS books;
    
    CREATE TABLE books (
      id SERIAL PRIMARY KEY,
      client TEXT NOT NULL,
      data JSONb NOT NULL
    );

# Populating the DB

    INSERT INTO books(client, data) values (
        'Joe', 
        '{ "title": "Siddhartha", "author": { "first_name": "Herman", "last_name": "Hesse" } }'
    ),(
        'Jenny', 
        '{ "title": "Dharma Bums", "author": { "first_name": "Jack", "last_name": "Kerouac" } }'
    ),(
        'Jenny', 
        '{ "title": "100 años de soledad", "author": { "first_name": "Gabo", "last_name": "Marquéz" } }'
    );

Lets see everything inside the table books:

    SELECT * FROM books;

Output:

![](http://i.imgur.com/T26elII.png)

# `->` operator returns values out of JSON columns

Selecting 1 column:

    SELECT client, 
        data->'title' AS title
        FROM books;
        
Output:

![enter image description here](http://i.imgur.com/Pab2puE.png)

Selecting 2 columns:

    SELECT client, 
       data->'title' AS title, data->'author' AS author
       FROM books;
       
Output:    

![enter image description here](http://i.imgur.com/fWHUsre.png)

# `->` vs `->>` 
The `->` operator returns the original JSON type (which might be an object), whereas `->>` returns text. 

# Return NESTED objects
You can use the `->` to return a nested object and thus chain the operators:

    SELECT client, 
       data->'author'->'last_name' AS author
       FROM books;
              
Output:

![enter image description here](http://i.imgur.com/NgSPIFU.png)

# Filtering
 Select rows based on a value inside your JSON:
 
     SELECT 
     client,
     data->'title' AS title
     FROM books
      WHERE data->'title' = '"Dharma Bums"';

Notice WHERE uses `->` so we must compare to JSON `'"Dharma Bums"'`

Or we could use `->>` and compare to `'Dharma Bums'`

Output:

![enter image description here](http://i.imgur.com/2seaUNK.png)

# Nested filtering
Find rows based on the value of a nested JSON object:

    SELECT 
     client,
     data->'title' AS title
     FROM books
      WHERE data->'author'->>'last_name' = 'Kerouac';

Output:

![enter image description here](http://i.imgur.com/yeBMj0T.png)

# A real world example

    CREATE TABLE events (
      name varchar(200),
      visitor_id varchar(200),
      properties json,
      browser json
    );
    
We’re going to store events in this table, like pageviews. Each event has properties, which could be anything (e.g. current page) and also sends information about the browser (like OS, screen resolution, etc). Both of these are completely free form and could change over time (as we think of extra stuff to track).


    INSERT INTO events (name, visitor_id, properties, browser) VALUES
    (
      'pageview', '1',
      '{ "page": "/" }',
      '{ "name": "Chrome", "os": "Mac", "resolution": { "x": 1440, "y": 900 } }'
    ),(
      'pageview', '2',
      '{ "page": "/" }',
      '{ "name": "Firefox", "os": "Windows", "resolution": { "x": 1920, "y": 1200 } }'
    ),(
      'pageview', '1',
      '{ "page": "/account" }',
      '{ "name": "Chrome", "os": "Mac", "resolution": { "x": 1440, "y": 900 } }'
    ),(
      'purchase', '5',
      '{ "amount": 10 }',
      '{ "name": "Firefox", "os": "Windows", "resolution": { "x": 1024, "y": 768 } }'
    ),(
      'purchase', '15',
      '{ "amount": 200 }',
      '{ "name": "Firefox", "os": "Windows", "resolution": { "x": 1280, "y": 800 } }'
    ),(
      'purchase', '15',
      '{ "amount": 500 }',
      '{ "name": "Firefox", "os": "Windows", "resolution": { "x": 1280, "y": 800 } }'
    );

Now lets select everything:

    SELECT * FROM events;
    
Output: 

![enter image description here](http://i.imgur.com/b5Hw0NN.png)

# JSON operators + PostgreSQL aggregate functions

Using the JSON operators, combined with traditional PostgreSQL aggregate functions, we can pull out whatever we want. You have the full might of an RDBMS at your disposal.

* Lets see browser usage: 

        SELECT browser->>'name' AS browser, 
          count(browser)
          FROM events
          GROUP BY browser->>'name';

Output:

![enter image description here](http://i.imgur.com/jvw6bz7.png)

* Total revenue per visitor:
    
        SELECT visitor_id, SUM(CAST(properties->>'amount' AS integer)) AS total
        FROM events
        WHERE CAST(properties->>'amount' AS integer) > 0
        GROUP BY visitor_id;

Output:

![enter image description here](http://i.imgur.com/6cOnNl9.png)

* Average screen resolution

        SELECT AVG(CAST(browser->'resolution'->>'x' AS integer)) AS width,
          AVG(CAST(browser->'resolution'->>'y' AS integer)) AS height
        FROM events;

Output:

![enter image description here](http://i.imgur.com/RfVELht.png)

More examples and documentation [here][1] and [here][2].


  [1]: http://schinckel.net/2014/05/25/querying-json-in-postgres/
  [2]: http://clarkdave.net/2013/06/what-can-you-do-with-postgresql-and-json/

## Querying complex JSON documents
Taking a complex JSON document in a table:


    CREATE TABLE mytable (data JSONB NOT NULL);
    CREATE INDEX mytable_idx ON mytable USING gin (data jsonb_path_ops);
    INSERT INTO mytable VALUES($$
    {
        "name": "Alice",
        "emails": [
            "alice1@test.com",
            "alice2@test.com"
        ],
        "events": [
            {
                "type": "birthday",
                "date": "1970-01-01"
            },
            {
                "type": "anniversary",
                "date": "2001-05-05"
            }
        ],
        "locations": {
            "home": {
                "city": "London",
                "country": "United Kingdom"
            },
            "work": {
                "city": "Edinburgh",
                "country": "United Kingdom"
            }
        }
    }
    $$);

Query for a top-level element:

    SELECT data->>'name' FROM mytable WHERE data @> '{"name":"Alice"}';

Query for a simple item in an array:

    SELECT data->>'name' FROM mytable WHERE data @> '{"emails":["alice1@test.com"]}';

Query for an object in an array:

    SELECT data->>'name' FROM mytable WHERE data @> '{"events":[{"type":"anniversary"}]}';

Query for a nested object:

    SELECT data->>'name' FROM mytable WHERE data @> '{"locations":{"home":{"city":"London"}}}';

## Performance of `@>` compared to `->` and `->>` ##
It is important to understand the performance difference between using `@>`, `->` and `->>` in the `WHERE` part of the query.  Although these two queries appear to be broadly equivalent:

    SELECT data FROM mytable WHERE data @> '{"name":"Alice"}';
    SELECT data FROM mytable WHERE data->'name' = '"Alice"';
    SELECT data FROM mytable WHERE data->>'name' = 'Alice';

the first statement will use the index created above whereas the latter two will not, requiring a complete table scan.

It is still allowable to use the `->` operator when obtaining resultant data, so the following queries will also use the index:

    SELECT data->'locations'->'work' FROM mytable WHERE data @> '{"name":"Alice"}';
    SELECT data->'locations'->'work'->>'city' FROM mytable WHERE data @> '{"name":"Alice"}';


## Creating a pure JSON table
To create a pure JSON table you need to provide a single field with the type `JSONB`:

    CREATE TABLE mytable (data JSONB NOT NULL);

You should also create a basic index:

    CREATE INDEX mytable_idx ON mytable USING gin (data jsonb_path_ops);

At this point you can insert data in to the table and query it efficiently.

