---
title: "Getting started with mongodb-csharp"
slug: "getting-started-with-mongodb-csharp"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Basic Operations
    class Program
        {
           
            static void Main(string[] args)
            {
                string serverName = "INTACT-ETL";
                string databaseName = "SOMEDB";
                string collectionName = "TestCol";
    
                //Build Connection string
                string connectionString = string.Format("mongodb://{0}", serverName);
                //Create MongoClient and connect in one step
                IMongoClient client = new MongoClient(connectionString);
                //Create database object
                IMongoDatabase database = client.GetDatabase(databaseName);
    
                //Inserting one object
                UserInfo userObject = new UserInfo {FirstName = "Bob",LastName = "Smith", DateOfBirth = DateTime.Now };
                IMongoCollection<UserInfo> collection = database.GetCollection<UserInfo>(collectionName);
                collection.InsertOne(userObject);
    
                //Retrieving one object
                UserInfo retrievedObject;
                FilterDefinition<UserInfo> theFilter = Builders<UserInfo>.Filter.Eq(p => p.FirstName, "Bob");
                retrievedObject = collection.Find<UserInfo>(theFilter).First();
                Console.WriteLine("FirstName: {0}", retrievedObject.FirstName);
                Console.WriteLine("LastName: {0}", retrievedObject.LastName);
                Console.WriteLine("DateOfBirth: {0}", retrievedObject.DateOfBirth);
    
                //Delete one object
                DeleteResult result =  collection.DeleteOne<UserInfo>(p => p.FirstName == "Bob");
                Console.WriteLine("Is Acked : {0}",result.IsAcknowledged);
                Console.WriteLine("Press Enter to exit...");
                Console.ReadLine();
    
    
            }
    
            public class UserInfo
            {
                [MongoDB.Bson.Serialization.Attributes.BsonId]
                public ObjectId? _id { get { return ObjectId.GenerateNewId(); } set { } }
                public string FirstName { get; set; }
                public string LastName { get; set; }
                public DateTime DateOfBirth { get; set; }
            }
        }

## How to connect MongoDb Database from application
 Define parameters

    private static IMongoClient _client;
    private static IMongoDatabase _database;
    private static IMongoCollection< -collection class name- > _collection;
Assign values to parameters

    _client = new MongoClient("mongodb://localhost:27017");
    _database = _client.GetDatabase("database name here");
    _collection = _database.GetCollection< -collection class name- >("collection name here");

## Installation or Setup
Download driver via nuget. Using this command in the package manager console

    Install-Package mongocsharpdriver

