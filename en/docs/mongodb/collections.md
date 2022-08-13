---
title: "Collections"
slug: "collections"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

Create Database

## Create a Collection
First Select Or Create a database.

    > use mydb
    switched to db mydb

Using `db.createCollection("yourCollectionName")` method you can explicitly create Collection.

    > db.createCollection("newCollection1")
    { "ok" : 1 }
Using `show collections` command see all collections in the database.

    > show collections
    newCollection1
    system.indexes
    > 

The `db.createCollection()` method has the following parameters:

|Parameter  |  Type  |  Description
|-----------|-----------|----------
name        |string        |The name of the collection to create.     
|options    |document    |*Optional.* Configuration options for creating a [capped collection][1] or for preallocating space in a new collection.

The fllowing example shows the syntax of `createCollection()` method with few important options

    >db.createCollection("newCollection4", {capped :true, autoIndexId : true, size : 6142800, max : 10000})
    { "ok" : 1 }

Both the `db.collection.insert()` and the `db.collection.createIndex()` operations create their respective collection if they do not already exist.

    > db.newCollection2.insert({name : "XXX"})
    > db.newCollection3.createIndex({accountNo : 1})
Now, Show All the collections using `show collections` command

    > show collections
    newCollection1
    newCollection2
    newCollection3
    newCollection4
    system.indexes

If you want to see the inserted document, use the `find()` command.

    > db.newCollection2.find()
    { "_id" : ObjectId("58f26876cabafaeb509e9c1f"), "name" : "XXX" }


  [1]: https://docs.mongodb.com/v3.2/reference/glossary/#term-capped-collection

## Drop Collection
MongoDB's `db.collection.drop()` is used to drop a collection from the database.

First, check the available collections into your database `mydb`.

    > use mydb
    switched to db mydb

    > show collections
    newCollection1
    newCollection2
    newCollection3
    system.indexes

Now drop the collection with the name `newCollection1`.

    > db.newCollection1.drop()
    true
**Note:** If the collection droped successfully then the method will return `true` otherwise it will return `false`.

Again check the list of collections into database.

    > show collections
    newCollection2
    newCollection3
    system.indexes

**Reference:** MongoDB [drop()][1] Method.


  [1]: https://docs.mongodb.com/manual/reference/method/db.collection.drop/

