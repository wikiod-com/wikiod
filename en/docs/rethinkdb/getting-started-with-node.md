---
title: "Getting started with Node"
slug: "getting-started-with-node"
draft: false
images: []
weight: 9996
type: docs
toc: true
---

## Installing the RethinkDB package from NPM
    npm install -g rethinkdb


## Making a connection to RethinkDB
 const r = require("rethinkdb");

    r.connect({host: 'localhost', port: 28015}, (conn) => console.log(conn))

    // Or as a promise
     
    let rdb_conn;
    r.connect({host: 'localhost', port: 28015}).then((conn) => {
      rdb_conn = conn;
    }).then(() => {
      // Continue to use rdb_conn
    });


## Listing all databases
    r.connect({host: 'localhost', port: 28015})
    .then((conn) => {
        return r.dbList().run(conn);
    }).then((result) => {
        // Prints out list of databases on the RethinkDB instance
        console.log(result);
    });


## Create a new database
    
    r.connect({host: 'localhost', port: 28015})
    .then((conn) => {
        return r.dbCreate("stackoverflow").run(conn);
    }).then((result) => {
        console.log(result);
    });

## Create a new table on a database
    r.connect({host: 'localhost', port: 28015})
    .then((conn) => {
        return r.db("stackoverflow").tableCreate("examples").run(conn);
    }).then((result) => {
        console.log(result);
    });

## Insert a document into a table
    r.connect({host: 'localhost', port: 28015})
    .then((conn) => {
        return r.db("stackoverflow").table("examples")
            .insert({
                // If `id` is not set, will automatically generate a UUID
                id: 1,
    
                name: 'Thinker',

                // Will translate Date types.
                creationDate: new Date(),

                // Embedded array
                tags: ['rethinkdb', 'rethinkdb-javascript', 'rethinkdb-python'],

                // Will evaluate `r.now()` using server time
                dateWithServerTime: r.now(),

                // Embedded document
                location: {
                    // Using geospatial example
                    coordinates: r.point(-122.423246,37.779388),
                    name: 'San Francisco'  
                },
            }).run(conn);
    }).then((result) => {
        // Returns results object which includes array of generated UUIDs 
        //  for inserted documents
        console.log(result);
    });

## Querying a document from a table
    r.connect({host: 'localhost', port: 28015})
    .then((conn) => {
        // Can also use .get({id: 1})
        return r.db("stackoverflow").table("examples").get(1).run(conn)
    }).then((result) => {
        console.log(result);
    })

