---
title: "CRUD Operations in MongoDB C#"
slug: "crud-operations-in-mongodb-c"
draft: false
images: []
weight: 9923
type: docs
toc: true
---

namespaces:

    using System.Collections.Generic;
    using System.Linq;

    using MongoDB.Bson;
    using MongoDB.Bson.Serialization.Attributes;
    using MongoDB.Driver;

    using IAggregateFluentExtensions = MongoDB.Driver.IAggregateFluentExtensions;
    using IMongoCollectionExtensions = MongoDB.Driver.IMongoCollectionExtensions;
    using MongoClient = MongoDB.Driver.MongoClient;



Class used in examples:

        public class Interactions
        {
            public ObjectId Id { get; set; }
            public string ChannelId { get; set; }
            public string ContactId { get; set; }
            public string Language { get; set; }
            public List<Pages> Pages { get; set; }
            public string SiteName { get; set; }
            public int Value { get; set; }
            public int VisitPageCount { get; set; }
        }


        public class Pages
        {
            public string Url { get; set; }
            public int VisitPageIndex { get; set; }
        }

## Insert a Document
    var client = new MongoClient("mongodb://localhost:27017");
    var database = client.GetDatabase("test");
    var collection = database.GetCollection < Interactions > ("Interactions");

    var newItem = new Interactions{
        SiteName = "Example",
        Pages = new List < Pages > {
            new Pages {
                Url =  @ "https://www.wikiod.com/docs/mongodb-csharp",
                VisitPageIndex = 4
            },
            new Pages {
                Url =  @ "https://github.com/",
                VisitPageIndex = 2
            },
        }
    };
    collection.InsertOne(newItem);

## Select a Document (Linq)
    var client = new MongoClient("mongodb://localhost:27017");
    var database = client.GetDatabase("test");
    var collection = database.GetCollection < Interactions > ("Interactions");   
    var result = IMongoCollectionExtensions
                .AsQueryable(collection)
                .FirstOrDefault(s => s.SiteName == "Example");

## Update a Document
var client = new MongoClient("mongodb://localhost:27017");
var database = client.GetDatabase("test");
var collection = database.GetCollection < Interactions > ("Interactions");    

var update = MongoDB.Driver
                .Builders<Interactions>
                .Update.Set(s => s.SiteName, "New Example");
 
collection.FindOneAndUpdate<Interactions>(s => s.SiteName == "Example", update);


## Delete a Document
    var client = new MongoClient("mongodb://localhost:27017");
    var database = client.GetDatabase("test");
    var collection = database.GetCollection < Interactions > ("Interactions");
        
    collection.DeleteOne(s => s.SiteName == "New Example");

