---
title: "Java Driver"
slug: "java-driver"
draft: false
images: []
weight: 9982
type: docs
toc: true
---

## Fetch Collection data with condition
 To get data from `testcollection` collection in `testdb` database where `name=dev`

    import org.bson.Document;
    import com.mongodb.BasicDBObject;
    import com.mongodb.MongoClient;
    import com.mongodb.ServerAddress;
    import com.mongodb.client.MongoCollection;
    import com.mongodb.client.MongoCursor;
    import com.mongodb.client.MongoDatabase;

    MongoClient mongoClient = new MongoClient(new ServerAddress("localhost", 27017));
    MongoDatabase db = mongoClient.getDatabase("testdb");
    MongoCollection<Document> collection = db.getCollection("testcollection");

    BasicDBObject searchQuery = new BasicDBObject();
    searchQuery.put("name","dev");

    MongoCursor<Document> cursor = collection.find(searchQuery).iterator();  
    try {
        while (cursor.hasNext()) {
            System.out.println(cursor.next().toJson());
        }
    } finally {
        cursor.close();
    }

## Create a tailable cursor
    find(query).projection(fields).cursorType(CursorType.TailableAwait).iterator();

That code applies to the ``MongoCollection`` class.

CursorType is an enum and it has the following values:

    Tailable
    TailableAwait

Corresponding to the old (<3.0) DBCursor addOption Bytes types:

    Bytes.QUERYOPTION_TAILABLE
    Bytes.QUERYOPTION_AWAITDATA






## Create a database user
To create a user **dev** with password **password123**

    MongoClient mongo = new MongoClient("localhost", 27017);
    MongoDatabase db =  mongo.getDatabase("testDB");
    Map<String, Object> commandArguments = new BasicDBObject();
    commandArguments.put("createUser", "dev");
    commandArguments.put("pwd", "password123");
    String[] roles = { "readWrite" };
    commandArguments.put("roles", roles);
    BasicDBObject command = new BasicDBObject(commandArguments);
    db.runCommand(command);

