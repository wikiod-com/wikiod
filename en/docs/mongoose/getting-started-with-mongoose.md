---
title: "Getting started with mongoose"
slug: "getting-started-with-mongoose"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation
Installing **mongoose** is as easy as running the `npm` command

`npm install mongoose --save`

But make sure you have also installed `MongoDB` for your OS or Have access to a MongoDB database.


----------


## Connecting to MongoDB database: ##

**1. Import mongoose into the app:**

    import mongoose from 'mongoose';


**2. Specify a Promise library:**

    mongoose.Promise = global.Promise;


**3. Connect to MongoDB:**

    mongoose.connect('mongodb://127.0.0.1:27017/database');

    /* Mongoose connection format looks something like this */
    mongoose.connect('mongodb://USERNAME:PASSWORD@HOST::PORT/DATABASE_NAME');


**Note:**
- By default mongoose connects to MongoDB at port `27017`, Which is the default port used by MongoDB.

- To connect to MongoDB hosted somewhere else, use the second syntax. Enter MongoDB username, password, host, port and database name.

MongoDB port is 27017 by default; use your app name as the db name.

## Connection with options and callback
Mongoose connect has 3 parameters, uri, options, and the callback function. To use them see sample below.
<!-- language: lang-js -->

    var mongoose = require('mongoose');
    
    var uri = 'mongodb://localhost:27017/DBNAME';

    var options = {
        user: 'user1',
        pass: 'pass'
    }

    mongoose.connect(uri, options, function(err){
        if (err) throw err;
        // if no error == connected
    });

