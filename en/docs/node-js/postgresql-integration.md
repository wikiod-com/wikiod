---
title: "PostgreSQL integration"
slug: "postgresql-integration"
draft: false
images: []
weight: 9984
type: docs
toc: true
---

## Connect To PostgreSQL
Using `PostgreSQL`npm module.  <br><br>
install dependency from npm <br>

    npm  install pg --save

Now you have to create a PostgreSQL connection, which you can later query.

Assume you Database_Name = students, Host = localhost and  DB_User= postgres

    var pg = require("pg")
    var connectionString = "pg://postgres:postgres@localhost:5432/students";
    var client = new pg.Client(connectionString);
    client.connect();

## Query with Connection Object
If you want to use connection object for query database you can use this sample code.

    var queryString = "SELECT name, age FROM students " ;
    var query = client.query(queryString);
    
    query.on("row", (row, result)=> {
    result.addRow(row);
    });
    
    query.on("end", function (result) {
    //LOGIC
    });



