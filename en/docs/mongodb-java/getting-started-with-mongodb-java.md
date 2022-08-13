---
title: "Getting started with mongodb-java"
slug: "getting-started-with-mongodb-java"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation or Setup
**Prerequisite**

A running MongoDB server, more details on that [here](https://www.wikiod.com/mongodb/getting-started-with-mongodb). 

**Setting Up a Java Project**

The MongoDB Java Driver is supplied as a JAR file and can be included in a project just like any other JAR. For example:
 
- Maven: add an entry to the `<dependencies>` section of your `pom.xml`.

>     <dependency>
>         <groupId>org.mongodb</groupId>
>         <artifactId>mongo-java-driver</artifactId>
>         <version>3.4.2</version>
>     </dependency> 

- Gradle: add the following dependency to your `build.gradle`

> `compile 'org.mongodb:mongo-java-driver:3.4.2'`

- Manual Inclusion: download the JAR file from [here](http://search.maven.org/#search%7Cga%7C1%7Corg.mongodb%3A%3Amongo-java-driver) and add it to your project's classpath.

Note: the version shown in the examples above is the latest stable version at the time of writing but you can choose any version available [here](http://search.maven.org/#search%7Cga%7C1%7Corg.mongodb%3A%3Amongo-java-driver).

**Using the MongoDB Java Driver**

Let's create a `MongoClient`, the simplest invocation is:

    MongoClient mongoClient = new MongoClient(new ServerAddress("localhost", 27017));

This presumes that the MongoDB server is running on localhost at the default port and that it is unsecured. There are numerous variants of this for authenticated access, custom connection options, connecting to replicasets and sharded clusters all covered in detail in the [Mongo DB Java Driver docs](http://mongodb.github.io/mongo-java-driver/3.4/driver/tutorials/connect-to-mongodb/).

Now that we're connected let's create a database and a collection. MongoDB implicitly creates a collection when the first document is saved into it so let's do that:

    MongoDatabase database = mongoClient.getDatabase("starter");
    MongoCollection<Document> collection = database.getCollection("superheroes");
    Document document = new Document("name", "Superman");
    collection.insertOne(document);

Note: you can [explicitly create a collection](http://mongodb.github.io/mongo-java-driver/3.4/driver/tutorials/databases-collections/) but the above usage is more common and certainly fits better with a 'getting started' topic.

Now let's see if we can find the document we created:

    FindIterable<Document> documents = purchases.find(Filters.eq("name", "Superman"));
    for (Document found : documents) {
        System.out.println(found.toJson());
    }

The above code will output the following (the `_id` field is MongoDB's auto generated document key):

> Found document: { "_id" : { "$oid" : "5981bd586d47c203904a9cf9" }, "name" : "Superman" }

Now that we've verified that we successfully created a document let's delete it:

    DeleteResult deleteResult = purchases.deleteOne(Filters.eq("name", "Superman"));
    System.out.println(String.format("Deleted %s document(s)", deleteResult.getDeletedCount()));

The above code will output the following:

> Deleted 1 document(s)


**Complete Example** 

Putting the various pieces together we get:

    import com.google.common.collect.Lists;
    import com.mongodb.MongoClient;
    import com.mongodb.MongoCredential;
    import com.mongodb.ServerAddress;
    import com.mongodb.client.FindIterable;
    import com.mongodb.client.MongoCollection;
    import com.mongodb.client.model.Filters;
    import com.mongodb.client.result.DeleteResult;
    import org.bson.Document;
    
    import java.util.List;
    
    public class IntroducingTheMongoDBJavaDriver {
    
        public static void main(String... args) throws Exception {
            MongoClient mongoClient = new MongoClient(new ServerAddress("localhost", 27017));
    
            MongoCollection<Document> superheroes = mongoClient.getDatabase("starter").getCollection("superheroes");

            Document document = new Document("name", "Superman");
            superheroes.insertOne(document);
    
            FindIterable<Document> documents = superheroes.find(Filters.eq("name", "Superman"));
            for (Document found : documents) {
                System.out.println(String.format("Found document: %s", found.toJson()));
            }
    
            DeleteResult deleteResult = superheroes.deleteOne(Filters.eq("name", "Superman"));
            System.out.println(String.format("Deleted %s document(s)", deleteResult.getDeletedCount()));
    
            long count = superheroes.count();
            System.out.println(String.format("Number of superheroes: %s", count));
        }
    }


