---
title: "Mongoose Queries"
slug: "mongoose-queries"
draft: false
images: []
weight: 9989
type: docs
toc: true
---

Mongoose is a Node.JS driver for MongoDB. It provides certain benefits over the default MongoDB driver, such as adding types to Schemas. One difference is that some Mongoose queries may differ from their MongoDB equivalents.

## Find One Query
Import a Mongoose Model (See topic "Mongoose Schemas")

`var User = require("../models/user-schema.js")`

The findOne method will return the first entry in the database that matches the first parameter. The parameter should be an object where the key is the field to look for and the value is the value to be matched.  This can use MongoDB query syntax, such as the dot (.) operator to search subfields.

    User.findOne({"name": "Fernando"}, function(err, result){
        if(err) throw err;    //There was an error with the database.
        if(!result) console.log("No one is named Fernando."); //The query found no results.
        else {
            console.log(result.name); //Prints "Fernando"
        }
    }

