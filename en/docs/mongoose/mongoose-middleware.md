---
title: "Mongoose Middleware"
slug: "mongoose-middleware"
draft: false
images: []
weight: 9989
type: docs
toc: true
---

In mongoose, **Middlewares** are also called as `pre` and `post` hooks.

## There are two types of middleware ## 
Both of these middleware support **pre** and **post** hooks.


1. **Document middleware**

    Its supported for document functions `init`, `validate`, `save` and `remove`


2. **Query middleware**

    Its supported for query functions `count`, `find`, `findOne`, `findOneAndRemove`, `findOneAndUpdate`, `insertMany` and `update`.

---

## Pre and Post hooks

There are two types of **Pre** hooks

1. **serial**

    As the name suggests, Its executed in serial order i..e one after another


2. **parallel**

   Parallel middleware offers more fine-grained flow control and the `hooked method` is not executed until `done` is called by all parallel middleware.


**Post** Middleware are executed after the `hooked method` and all of its `pre` middleware have been completed.

---

**hooked methods** are the functions supported by document middleware. `init, validate, save, remove`

## Middleware to hash user password before saving it
*This is an example of **`Serial`** **`Document Middleware`***

In this example, We will write a middleware that will convert the plain text password into a hashed password before saving it in database.

This middleware will automatically kick in when creating new user or updating existing user details.


---


**FILENAME :** `User.js`

<!-- language: lang-js -->
    // lets import mongoose first
    import mongoose from 'mongoose'
    
    // lets create a schema for the user model
    const UserSchema = mongoose.Schema(
      {
        name: String,
        email: { type: String, lowercase: true, requird: true },
        password: String,
      },
    );
    
    
    /**
     * This is the middleware, It will be called before saving any record
     */
    UserSchema.pre('save', function(next) {
    
      // check if password is present and is modified.
      if ( this.password && this.isModified('password') ) {
    
        // call your hashPassword method here which will return the hashed password.
        this.password = hashPassword(this.password);
    
      }
    
      // everything is done, so let's call the next callback.
      next();
    
    });
    
    
    // lets export it, so we can import it in other files.
    export default mongoose.model( 'User', UserSchema );


Now every time we save a user, This middleware will check if password is **set** and **is modified** (this is so we dont hash users password if its not modified.)

---

**FILENAME :** `App.js`
<!-- language: lang-js -->

    import express from 'express';
    import mongoose from 'mongoose';
    
    // lets import our User Model
    import User from './User';
    
    // connect to MongoDB
    mongoose.Promise = global.Promise;
    mongoose.connect('mongodb://127.0.0.1:27017/database');
    
    
    let app = express();
    /* ... express middlewares here .... */
    
    
    app.post( '/', (req, res) => {
    
      /*
        req.body = {
          name: 'John Doe',
          email: 'john.doe@gmail.com',
          password: '!trump'
        }
       */
    
      // validate the POST data
    
      let newUser = new User({
        name: req.body.name,
        email: req.body.email,
        password: req.body.password,
      });
    
      newUser.save( ( error, record ) => {
        if (error) {
          res.json({
            message: 'error',
            description: 'some error occoured while saving the user in database.'
          });
        } else {
          res.json({
            message: 'success',
            description: 'user details successfully saved.',
            user: record
          });
        }
      });
    
    });
    
    
    
    let server = app.listen( 3000, () => {
        console.log(`Server running at http://localhost:3000` );
      }
    );








