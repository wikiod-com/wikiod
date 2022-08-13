---
title: "Learning Elasticsearch with kibana"
slug: "learning-elasticsearch-with-kibana"
draft: false
images: []
weight: 9991
type: docs
toc: true
---

Kibana is front end data visualization tool for elasticsearch. for installing kibana refer to the kibana documentation. For running kibana on localhost go to https://localhost:5601 and go to kibana console.





## Explore your Cluster using Kibana
 The command syntax will be of the following type:

    <REST Verb> /<Index>/<Type>/<ID>

Execute the following command to explore elasticsearch cluster through Kibana Console. 
 - For checking the cluster health 
    

    GET /_cat/health?v

 - For listing all the indices
    

    GET /_cat/indices?v

 - For creating an index with name car


     PUT /car?pretty

 - For indexing the document with name car of external type using id 1


    PUT /car/external/1?pretty
    {
      "name": "Tata Nexon"
    }

  the response of above query will be :
    
    {
      "_index": "car",
      "_type": "external",
      "_id": "1",
      "_version": 1,
      "result": "created",
      "_shards": {
        "total": 2,
        "successful": 1,
        "failed": 0
      },
      "created": true
    }

 - retrieving the above document can be done using:


    GET /car/external/1?pretty

 - For deleting an index


    DELETE /car?pretty



## Modify your elasticsearch data
Elasticsearch provides data manipulation & data searching capabilities in almost real time. under this example, we have update, delete & batch processing operations. 

 - Updating the same document. Suppose we have already indexed a
   document on /car/external/1 . Then running the command for indexing
   the data replaces the previous document.

    

    PUT /car/external/1?pretty
    {
      "name": "Tata Nexa"
    }

previous car document at id 1 with name "Tata Nexon" will be updated with new name "Tata Nexa"

 - indexing the data with explicit Id


    POST /car/external?pretty
    {
      "name": "Jane Doe"
    }

> for indexing the document without an Id we use **POST** verb instead
> of **PUT** verb. if we don't provide an Id, elasticsearch will
> generate a random ID and then use it to index the document.

 - Updating the previous document at an Id partially.


    POST /car/external/1/_update?pretty
    {
      "doc": { "name": "Tata Nex" }
    }
 - updating the document with additional information


    POST /car/external/1/_update?pretty
    {
      "doc": { "name": "Tata Nexon", "price": 1000000 }
    }

- updating the document using simple scripts. 


    POST /car/external/1/_update?pretty
    {
      "script" : "ctx._source.price += 50000"
    }

   

> ctx._source refers to the current source document that is about to be
> updated. Above script provides only one script to be updated at the same time.

 - Deleting the document


    DELETE /car/external/1?pretty

> Note: deleting a whole index is more efficient than deleting all
> documents by using Delete by Query API

**Batch Processing** 

Apart from indexing updating & deleting the document, elasticsearch also provides provides the ability to perform any of the above operations in batches using the **_bulk** API. 

 - for updating multiple documents using **_bulk** API


    POST /car/external/_bulk?pretty
    {"index":{"_id":"1"}}
    {"name": "Tata Nexon" }
    {"index":{"_id":"2"}}
    {"name": "Tata Nano" }
 - for updating & deleting the documents using **_bulk** API


    POST /car/external/_bulk?pretty
    {"update":{"_id":"1"}}
    {"doc": { "name": "Tata Nano" } }
    {"delete":{"_id":"2"}}

> If an operation fails, bulk API doesn't stop. It executes all the
> operations & finally returns report for all the operations.



