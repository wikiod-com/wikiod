---
title: "Publishing Data"
slug: "publishing-data"
draft: false
images: []
weight: 9790
type: docs
toc: true
---

Within Meteor's data subsystem, a server publication and its corresponding client subscriptions are the main mechanisms of reactive, live data transport where the underlying data is constantly synchronized between the server and the client.

## Global publications
A global publication does not possess a name and does not require a subscription from the connected client and therefore it is available to the connected client as soon as the client connects to the server.

To achieve this, one simply names the publication as `null` like so
<!-- language: lang-js -->
```
Meteor.publish(null, function() {
  return SomeCollection.find();
})
```

## Creating and responding to an error on a publication.
On the server, you can create a publication like this. `this.userId` is the id of the user who is currently logged in. If no user is logged in, you might want to throw an error and respond to it.

<!-- language: lang-js -->

    import Secrets from '/imports/collections/Secrets';

    Meteor.publish('protected_data', function () {
      if (!this.userId) {
        this.error(new Meteor.Error(403, "Not Logged In."));
        this.ready();
      } else {
        return Secrets.find();
      }
    });

On the client, you can respond with the following.

<!-- language: lang-js -->

    Meteor.subscribe('protected_data', {
      onError(err) {
        if (err.error === 403) {
          alert("Looks like you're not logged in");
        }
      },
    });

File /imports/collections/Secrets creates reference to the secrets collection as below:
<!-- language: lang-js -->
    const Secrets = new Mongo.Collection('secrets');

## Template scoped subscriptions
Meteor's default templating system Spacebars and its underlying rendering subsystem Blaze integrate seemlessly with publication lifecycle methods such that a simple piece of template code can subscribe to its own data, stop and clean up its own traces during the template tear down.

In order to tap into this, one needs to subscribe on the template instance, rather than the `Meteor` symbol like so:


First set up the template
<!-- language: lang-spacebars -->
```
<template name="myTemplate">
  We will use some data from a publication here
</template>
``` 

Then tap into the corresponding lifecycle callback
<!-- language: lang-js -->
```
Template.myTemplate.onCreated(function() {
  const templateInstance = this;
  templateInstance.subscribe('somePublication')
})
```

Now when this template gets destroyed, the publication will also be stopped automatically.

Note: The data that is subscribed to will be available to all templates.

## Named publications
A named publication is one that possesses a name and needs to be explicitly subscribed to from the client.

Consider this server side code:
<!-- language: lang-js -->
```
Meteor.publish('somePublication', function() {
  return SomeCollection.find()
})
```

The client needs to request it by:
<!-- language: lang-js -->
```
Meteor.subscribe('somePublication')
```

## Validating User Account On Publish
Sometimes it's a good idea to further secure your publishes by requiring a user login. Here is how you achieve this via Meteor.

<!-- language: lang-js -->

    import { Recipes } from '../imports/api/recipes.js';
    import { Meteor } from 'meteor/meteor';
    
    Meteor.publish('recipes', function() {
      if(this.userId) {
        return Recipe.find({});
      } else {
        this.ready();  // or: return [];
      }
    });



## Basic Subscription and Publication
First, remove `autopublish`. `autopublish` automatically publishes the entire database to the client-side, and so the effects of publications and subscriptions cannot be seen.

To remove `autopublish`:

    $ meteor remove autopublish

Then you can create publications. Below is a full example.

<!-- language: lang-js -->

    import { Mongo } from 'meteor/mongo';
    import { Meteor } from 'meteor/meteor';
    
    const Todos = new Mongo.Collection('todos');
    
    const TODOS = [
      { title: 'Create documentation' },
      { title: 'Submit to Stack Overflow' }
    ];
    
    if (Meteor.isServer) {
      Meteor.startup(function () {
        TODOS.forEach(todo => {
          Todos.upsert(
            { title: todo.title },
            { $setOnInsert: todo }
          );
        });
      });
    
      // first parameter is a name.
      Meteor.publish('todos', function () {
        return Todos.find();
      });
    }
    
    if (Meteor.isClient) {
      // subscribe by name to the publication.
      Meteor.startup(function () {
        Meteor.subscribe('todos');
      })
    }

## Publish into an ephemeral client-side named collection.
For if you have to fine-tune what is published.

<!-- language: lang-js -->

    import { Mongo } from 'meteor/mongo';
    import { Meteor } from 'meteor/meteor';
    import { Random } from 'meteor/random';
    
    if (Meteor.isClient) {
      // established this collection on the client only.
      // a name is required (first parameter) and this is not persisted on the server.
      const Messages = new Mongo.Collection('messages');
      Meteor.startup(function () {
        Meteor.subscribe('messages');
        Messages.find().observe({
          added: function (message) {
            console.log('Received a new message at ' + message.timestamp);
          }
        });
      })
    }
    
    if (Meteor.isServer) {
      // this will add a new message every 5 seconds.
      Meteor.publish('messages', function () {
        const interval = Meteor.setInterval(() => {
          this.added('messages', Random.id(), {
            message: '5 seconds have passed',
            timestamp: new Date()
          })
        }, 5000);
        this.added('messages', Random.id(), {
          message: 'First message',
          timestamp: new Date()
        });
        this.onStop(() => Meteor.clearInterval(interval));
      });
    }

## Reactively re-subscribing to a publication
A template autorun may be used to (re)subscribe to a publication. It establishes a reactive context which is re-executed whenever any *reactive data it depends on changes*. In addition, an autorun always runs once (the first time it is executed).

Template autoruns are normally put in an `onCreated` method.
<!-- language: lang-js -->
```
Template.myTemplate.onCreated(function() {
  this.parameter = new ReactiveVar();
  this.autorun(() => {
    this.subscribe('myPublication', this.parameter.get());
  });
});
```

This will run once (the first time) and set up a subscription. It will then re-run whenever the `parameter` reactive variable changes.

## Wait in the Blaze view while published data is being fetched
Template JS code

<!-- language: lang-js -->

    Template.templateName.onCreated(function(){
        this.subscribe('subsription1');
        this.subscribe('subscription2');
    });


Template HTML code

<!-- language: lang-spacebars -->

    <template name="templateName">
        {{#if Template.subscriptionsReady }}
            //your actual view with data. it can be plain HTML or another template
        {{else}}
            //you can use any loader or a simple header
            <h2> Please wait ... </h2>
        {{/if}}
    </template>

## Simulate delay in publications
In real world, connection and server delays could occur, to simulate delays in development environment `Meteor._sleepForMs(ms);` could be used

<!-- language: lang-js -->

    Meteor.publish('USER_DATA', function() {
        Meteor._sleepForMs(3000); // Simulate 3 seconds delay
        return Meteor.users.find({});
    });
    

## Publish multiple cursors
Multiple database cursors can be published from the same publication method by returning an array of cursors.

The "children" cursors will be treated as joins and will not be reactive.

<!-- language: lang-js -->

    Meteor.publish('USER_THREAD', function(postId) {
      let userId   = this.userId;
    
      let comments = Comments.find({ userId, postId });
      let replies  = Replies.find({ userId, postId });
    
      return [comments, replies];
    });

## Merging Publications
Publications can be merged on the client, resulting in differently shaped documents within a single cursor.  The following example represents how a user directory might publish a minimal amount of public data for users of an app, and provide a more detailed profile for the logged in user.

<!-- language: lang-js -->
```
// client/subscriptions.js  
Meteor.subscribe('usersDirectory');
Meteor.subscribe('userProfile', Meteor.userId());

// server/publications.js  
// Publish users directory and user profile

Meteor.publish("usersDirectory", function (userId) {
    return Meteor.users.find({}, {fields: {
        '_id': true,
        'username': true,
        'emails': true,
        'emails[0].address': true,

        // available to everybody
        'profile': true,
        'profile.name': true,
        'profile.avatar': true,
        'profile.role': true
    }});
});
Meteor.publish('userProfile', function (userId) {
    return Meteor.users.find({_id: this.userId}, {fields: {
        '_id': true,
        'username': true,
        'emails': true,
        'emails[0].address': true,
    
        'profile': true,
        'profile.name': true,
        'profile.avatar': true,
        'profile.role': true,
    
        // privately accessible items, only availble to the user logged in
        'profile.visibility': true,
        'profile.socialsecurity': true,
        'profile.age': true,
        'profile.dateofbirth': true,
        'profile.zip': true,
        'profile.workphone': true,
        'profile.homephone': true,
        'profile.mobilephone': true,
        'profile.applicantType': true
    }});
});
```

