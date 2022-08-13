---
title: "Cassandra Integration"
slug: "cassandra-integration"
draft: false
images: []
weight: 9985
type: docs
toc: true
---

## Hello world
For accessing Cassandra [`cassandra-driver`](https://github.com/datastax/nodejs-driver) module from DataStax can be used. It supports all the features and can be easily configured. 

    const cassandra = require("cassandra-driver");
    const clientOptions = {
        contactPoints: ["host1", "host2"],
        keyspace: "test"
    };
    
    const client = new cassandra.Client(clientOptions);
    
    const query = "SELECT hello FROM world WHERE name = ?";
    client.execute(query, ["John"], (err, results) => {
        if (err) {
          return console.error(err);
        }
    
        console.log(results.rows);
    });

