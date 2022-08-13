---
title: "using thinky.io with RethinkDB"
slug: "using-thinkyio-with-rethinkdb"
draft: false
images: []
weight: 9994
type: docs
toc: true
---

## Starting thinky in node.js
thinky is a lightweight node.js ORM for RethinkDB.

First you need to have RethinkDB running on your server.

Then install the thinky.io npm package into your project.

    npm install --save thinky

Now import thinky into your model file.

    const thinky = require('thinky)();
    const type = thinky.type

Next create a model.

    const User = thinky.createModel('User' {
        email:type.string(),
        password: type.string()
    });

You can now create and save a user.

    const user = new User({
        email: 'test@email.com',
        password: 'password'
    });

    user.save();

The user will be given a unique id.

You can chain a promise onto the save function to use the resulting user, or catch an error.

    user.save()
        .then(function(result) {
            console.log(result);
        })
        .catch(function(error) {
            console.log(error);
        });

Find your user using functions such as filter, and use promises to use the results.

    User.filter({ email: 'test@email.com }).run()
        .then(function(result) {
            console.log(result);
        })
        .catch(function(error) {
            console.log(error);
        });

Or search for a specific user by the unique id.

    User.get(id)
        .then(function(user) {
            console.log(user);
        })
        .catch(function(error) {
            console.log(error);
        });

        




