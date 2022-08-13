---
title: "Getting started with PyMongo"
slug: "getting-started-with-pymongo"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Hello, World
PyMongo is a native Python driver for MongoDB.

## Install PyMongo

    pip install pymongo

## Create a connection
Use MongoClient to create a connection. MongoClient defaults to MongoDB instance running on `localhost:27017` if not specified.

    from pymongo import MongoClient
    client = MongoClient() 

## Access Database Objects
PyMongo's [Database][1] class represents database construct in MongoDB. Databases hold groups of logically related collections.

    db = client.mydb

## Access Collection Objects
PyMongo's [Collection][2] class represents collection construct in MongoDB. Collections hold groups of related documents.

    col = db.mycollection
    
MongoDB creates new databases and collections implicitly upon first use.


  [1]: http://api.mongodb.com/python/current/api/pymongo/database.html
  [2]: http://api.mongodb.com/python/current/api/pymongo/collection.html

## Basic CRUD Operation
MongoDB stores data records as [BSON][1] *documents*. BSON is the binary representation of JSON.

  [1]: http://bsonspec.org/
    
    $ python
    >>> from pymongo import MongoClient
    >>> client = MongoClient()
    >>> col = client.mydb.test


## Create
Insert a single document `insert_one(document)`

    >>> result = col.insert_one({'x':1})
    >>> result.inserted_id
    ObjectId('583c16b9dc32d44b6e93cd9b')

Insert multiple documents `insert_many(documents)`

    >>> result = col.insert_many([{'x': 2}, {'x': 3}])
    >>> result.inserted_ids
    [ObjectId('583c17e7dc32d44b6e93cd9c'), ObjectId('583c17e7dc32d44b6e93cd9d')]

Replace a single document matching the filter `replace_one(filter, replacement, upsert=False)`. 
(to insert a new document if matching document doesn't exist, use `upsert=True`)

    >>> result = col.replace_one({'x': 1}, {'y': 1})
    >>> result.matched_count
    1
    >>> result.modified_count
    1
    

## Update
Update a single document matching the filter `update_one(filter, update, upsert=False)`

    >>> result = col.update_one({'x': 1}, {'x': 3})

Update one or more documents that match the filter `update_many(filter, update, upsert=False)`

    >>> result = col.update_many({'x': 1}, {'x': 3})


## Read
Query the database `find(filter=None, projection=None, skip=0, limit=0, no_cursor_timeout=False)`. The *filter* argument is a prototype document that all results must match.

    >>> result = col.find({'x': 1})

Get a single document from the database `find_one(filter=None)`

    >>> result = col.find_one()

### Query With Projection

    query={'x':1}
    projection={'_id':0, 'x':1} # show x but not show _id
    result=col.find(query,projection)


## Delete
Delete a single document matching the filter `delete_one(filter)`

    >>> result = col.delete_one({'x': 1})
    >>> result.deleted_count
    1

Delete one or more documents matching the filter `delete_many(filter)`

    >>> result = col.delete_many({'x': 1})
    >>> result.deleted_count
    3

PyMongo also provides `find_one_and_delete()`, `find_one_and_update()` and `find_one_and_replace()` functionality.

## Installation or Setup
Detailed instructions on getting pymongo set up or installed.

 - Installing with [Pip][1]

     - To install pymongo for the first time:
    
        `pip install pymongo`
    
     - Installing a specific version of pymongo:
        
        Where X.X.X is the version to be installed
    
        `pip install pymongo==X.X.X`
    
     - Upgrading existing pymongo:
    
        `pip install --upgrade pymongo`

 - Installing with [easy_install][2]

    - To install pymongo for the first time:
    
        `python -m easy_install pymongo`

    - Upgrading existing pymongo:

        `python -m easy_install -U pymongo`


  [1]: https://pip.pypa.io/en/stable/installing/
  [2]: https://pypi.python.org/pypi/setuptools

