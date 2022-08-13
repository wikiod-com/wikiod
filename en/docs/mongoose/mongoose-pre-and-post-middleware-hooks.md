---
title: "mongoose pre and post middleware (hooks)"
slug: "mongoose-pre-and-post-middleware-hooks"
draft: false
images: []
weight: 9835
type: docs
toc: true
---

## Middleware
Middleware (also called pre and post hooks) are functions which are passed control during execution of asynchronous functions. Middleware is specified on the schema level and is useful for writing plugins. Mongoose 4.0 has 2 types of middleware: document middleware and query middleware. Document middleware is supported for the following document functions.

 - init 
 - validate 
 - save 
 - remove

Query middleware is supported for the following Model and Query functions.

 - count
 - find
 - findOne
 - findOneAndRemove
 - findOneAndUpdate
 - update

Both document middleware and query middleware support pre and post hooks. 
<hr/>

**Pre**

There are two types of pre hooks, serial and parallel.

**Serial**

Serial middleware are executed one after another, when each middleware calls next.

    var schema = new Schema(..);
    schema.pre('save', function(next) {
      // do stuff
      next();
    });

**Parallel**

Parallel middleware offer more fine-grained flow control.

var schema = new Schema(..);

    // `true` means this is a parallel middleware. You **must** specify `true`
    // as the second parameter if you want to use parallel middleware.
    schema.pre('save', true, function(next, done) {
      // calling next kicks off the next middleware in parallel
      next();
      setTimeout(done, 100);
    });

The hooked method, in this case save, will not be executed until done is called by each middleware.
<hr/>

**Post middleware**

post middleware are executed after the hooked method and all of its pre middleware have completed. post middleware do not directly receive flow control, e.g. no next or done callbacks are passed to it. post hooks are a way to register traditional event listeners for these methods.

    schema.post('init', function(doc) {
      console.log('%s has been initialized from the db', doc._id);
    });
    schema.post('validate', function(doc) {
      console.log('%s has been validated (but not saved yet)', doc._id);
    });
    schema.post('save', function(doc) {
      console.log('%s has been saved', doc._id);
    });
    schema.post('remove', function(doc) {
      console.log('%s has been removed', doc._id);
    });



