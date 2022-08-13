---
title: "MongoDB"
slug: "mongodb"
draft: false
images: []
weight: 9945
type: docs
toc: true
---

MongoDB is a free and open-source cross-platform document orient database program. Unlike classic SQL databases, MongoDB uses BSON (like JSON) to store data. Meteor was designed to use MongoDB for database storage and this topic explains how to implement MongoDB storage into Meteor applications.

## Export a Remote Mongo DB, Import Into a Local Meteor Mongo DB
Helpful when you want to grab a copy of a production database to play around with locally.

1. `mongodump --host some-mongo-host.com:1234 -d DATABASE_NAME -u DATABASE_USER -p DATABASE_PASSWORD` This will create a local `dump` directory; within that directory you'll see a directory with your `DATABASE_NAME`.
2. With your local meteor app running, from within the `dump` directory, run: `mongorestore --db meteor --drop -h localhost --port 3001 DATABASE_NAME`

## Get the Mongo URL of Your Local Meteor Mongo DB
While your Meteor app is running locally:

```
meteor mongo --url
```

## Connect Your Local Meteor App to an Alternative Mongo DB
Set the `MONGO_URL` environment variable before starting your local Meteor app.

## Linux/MacOS Example:

```
MONGO_URL="mongodb://some-mongo-host.com:1234/mydatabase" meteor
```

or

```
export MONGO_URL="mongodb://some-mongo-host.com:1234/mydatabase" 
meteor
```

## Windows Example

Note: don't use `"`

```
set MONGO_URL=mongodb://some-mongo-host.com:1234/mydatabase
meteor
```

## NPM

```
//package.json

"scripts": {
    "start": "MONGO_URL=mongodb://some-mongo-host.com:1234/mydatabase meteor"
}

$ npm start
```


## Running Meteor without MongoDB
Set `MONGO_URL` to any arbitrary value except for a database URL and ensure no collections are defined in your Meteor project (including collections defined by Meteor packages) to run Meteor without MongoDB.

Note that without MongoDB, server/client methods alongside any packages related to Meteor's user-account system will be undefined. Ex: `Meteor.userId()`

**Linux/Mac:**

    MONGO_URL="none" meteor

*or*

    export MONGO_URL="none"
    meteor
    
**Windows:**

    set MONGO_URL=none
    meteor

## Getting Started
You can start the **`mongo`** shell by running the following command inside your Meteor project:

    meteor mongo

**Please note:** Starting the  server-side database console only works while Meteor is running the application locally.

After that, you can list all collections by executing the following command via the **`mongo`** shell:

    show collections

You can also run basic MongoDB operations, like querying, inserting, updating and deleting documents.

-----

# Query Documents

Documents can be queried by using the `find()` method, e.g.:

    db.collection.find({name: 'Matthias Eckhart'});

This will list all documents that have the `name` attribute set to `Matthias Eckhart`.

# Inserting Documents

If you want to insert documents in a collection, run:

    db.collection.insert({name: 'Matthias Eckhart'});

# Updating Documents

In case you want to update documents, use the `update()` method, for instance:

    db.collection.update({name: 'Matthias Eckhart'}, {$set: {name: 'John Doe'}});

Executing this command will update a **single** document by setting the value `John Doe` for the field `name` (initially the value was `Matthias Eckhart`).

If you want to update **all** documents that match a specific criteria, set the `multi` parameter to `true`, for example:

    db.collection.update({name: 'Matthias Eckhart'}, {$set: {name: 'John Doe'}}, {multi: true});


Now, all documents in the collection that had initially the `name` attribute set to `Matthias Eckhart` have been updated to `John Doe`.

# Deleting Documents

Documents can be easily removed by using the `remove()` method, for example:

    db.collection.remove({name: 'Matthias Eckhart'});

This will remove all documents that match the value specified in the `name` field.

