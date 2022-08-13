---
title: "Using nedb in electron"
slug: "using-nedb-in-electron"
draft: false
images: []
weight: 9997
type: docs
toc: true
---

## Installation of nedb
It's very easy to install nedb.

    npm install nedb --save    # Put latest version in your package.json

For bower loving people,

    bower install nedb    



## Connecting electron app with Nedb
While building electron apps, usually the backend is in separate folder (js files) and front end is in a separate folder (html files). In the backend, in order to use the database, we have to include the nedb package with the require statement as follows.

    var Datastore = require('nedb'),db = new Datastore({ filename: 'data.db', autoload: true });

Keep in mind that the loading of the database file is an asynchronous task.



## Insert Data in nedb
Basically, in order to insert records to nedb, the data is stored in the form of json with the key being the column names and the value for those names will be the values for that record.

    var rec = { name: 'bigbounty',age:16};
    
    db.insert(rec, function (err, newrec) {   // Callback is optional
      // newrec is the newly inserted document, including its _id
      // newrec has no key called notToBeSaved since its value was undefined
    });

Be careful with all the operations of database, as they are asynchronous.

**Note** ** : If _id is not there in the json data that you are inserting then automatically ,it will be created for you by nedb.

## Search in nedb
In order to search for records in nedb, again we need to just pass the json containing the search criteria as a parameter to the find function of db object.

    db.find({ name: 'bigbounty' }, function (err, docs) {
      // docs is an array containing documents that have name as bigbounty
      // If no document is found, docs is equal to []
    });

In order to find only one document, as in we use limit in mysql, it's easy in nedb.

    db.findOne({ name: 'bigbounty' }, function (err, doc) {
      // doc is only one document that has name as bigbounty
      // If no document is found, docs is equal to []
    });



## Delete in nedb
In order to remove documents in nedb, it's very easy. We just have to use remove function of db object.

db.remove({ name: 'bigbounty' }, function (err, numremoved) {
  // numremoved is the number of documents that are removed.
});

