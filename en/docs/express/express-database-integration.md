---
title: "Express Database Integration"
slug: "express-database-integration"
draft: false
images: []
weight: 9987
type: docs
toc: true
---

## Connect to MongoDB with Node & Express
First, ensure you have installed *mongodb* and *express* via npm. Then, in a file conventionally titled *db.js*, use the following code:

    var MongoClient = require('mongodb').MongoClient

    var state = {
        db: null,
    }

    exports.connect = function(url, done) {
        if (state.db) return done()

        MongoClient.connect(url, function(err, db) {
            if(err) return done(err)
            state.db = db
            done()
        })
    }

    exports.get = function() {
        return state.db
    }

    exports.close = function(done) {
        if (state.db) {
            state.db.close(function(err, result) {
                state.db = null;
                state.mode = null;
                done(err);
            })
        } 
    }

This file will connect to the database and then you can just use the **db** object returned by the **get** method.

Now you need to include the db file by requiring it in you app.js file. Assuming your *db.js* file is in the same directory as *app.js* you can insert the line:

    var db = require('./db');

This, however, does not actually connect you to your MongoDB instance. To do that insert the following code before your app.listen method is called. In our example we integrate error handling and the app.listen method into the database connection. Please note this code only works if you are running your mongo instance on the same machine you Express app is located on.

    db.connect('mongodb://localhost:27017/databasename', function(err) {
        if (err) {
            console.log('Unable to connect to Mongo.');
            process.exit(1);
        } else {
            app.listen(3000, function() {
                console.log('Listening on port 3000...');
            });
        }
    });

There you go, your Express app should now be connected to your Mongo DB. Congrats!

