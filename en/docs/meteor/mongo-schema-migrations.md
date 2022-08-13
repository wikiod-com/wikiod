---
title: "Mongo Schema Migrations"
slug: "mongo-schema-migrations"
draft: false
images: []
weight: 9983
type: docs
toc: true
---

It's often necessary to run maintenance scripts on your database. Fields get renamed; data structures get changed; features that you used to support get removed; services get migrated. The list of reasons why you might want to change your schema is pretty limitless. So, the 'why' is pretty self explanatory.

The 'how' is a little more unfamiliar. For those people accustomed to SQL functions, the above database scripts will look strange. But notice how they're all in javascript, and how they're using the same API as we use throughout Meteor, on both the server and client. We have a consistent API through our database, server, and client. 

Run the schema migration commands from the meteor mongo shell:

```
# run meteor
meteor

# access the database shell in a second terminal window
meteor mongo
```

## Add Version Field To All Records in a Collection
```
db.posts.find().forEach(function(doc){
    db.posts.update({_id: doc._id}, {$set:{'version':'v1.0'}}, false, true);
});
```

## Remove Array From All Records In A Collection
```
db.posts.find().forEach(function(doc){
    if(doc.arrayOfObjects){
        // the false, true at the end refers to $upsert, and $multi, respectively   
        db.accounts.update({_id: doc._id}, {$unset: {'arrayOfObjects': "" }}, false, true);
    }
});
```

## Rename Collection
```
db.originalName.renameCollection("newName" );
```

## Find Field Containing Specific String
With the power of regex comes great responsibility....

```
db.posts.find({'text': /.*foo.*|.*bar.*/i})
```

## Create New Field From Old
```
db.posts.find().forEach(function(doc){
    if(doc.oldField){
        db.posts.update({_id: doc._id}, {$set:{'newField':doc.oldField}}, false, true);
    }
});
```

## Pull Objects Out of an Array and Place in a New Field
```
db.posts.find().forEach(function(doc){
    if(doc.commenters){
        var firstCommenter = db.users.findOne({'_id': doc.commenters[0]._id });
        db.clients.update({_id: doc._id}, {$set:{'firstPost': firstCommenter }}, false, true);

        var firstCommenter = db.users.findOne({'_id': doc.commenters[doc.commenters.length - 1]._id });
        db.clients.update({_id: doc._id}, {$set:{'lastPost': object._id }}, false, true);
    }
});
```

## Blob Record From One Collection Into Another Collection (ie. Remove Join & Flatten)
```
db.posts.find().forEach(function(doc){
    if(doc.commentsBlobId){
        var commentsBlob = db.comments.findOne({'_id': commentsBlobId });
        db.posts.update({_id: doc._id}, {$set:{'comments': commentsBlob }}, false, true);
    }
});
```

## Make Sure Field Exists
```
db.posts.find().forEach(function(doc){
    if(!doc.foo){
        db.posts.update({_id: doc._id}, {$set:{'foo':''}}, false, true);
    }
});
```

## Make Sure Field has Specific Value
```
db.posts.find().forEach(function(doc){
    if(!doc.foo){
        db.posts.update({_id: doc._id}, {$set:{'foo':'bar'}}, false, true);
    }
});
```

## Remove Record if Specific Field is Specific Value
```
db.posts.find().forEach(function(doc){
    if(doc.foo === 'bar'){
        db.posts.remove({_id: doc._id});
    }
});

```

## Change Specific Value of Field to New Value
```
db.posts.find().forEach(function(doc){
    if(doc.foo === 'bar'){
        db.posts.update({_id: doc._id}, {$set:{'foo':'squee'}}, false, true);
    }
});
```

## Unset Specific Field to Null
```
db.posts.find().forEach(function(doc){
    if(doc.oldfield){
        // the false, true at the end refers to $upsert, and $multi, respectively
        db.accounts.update({_id: doc._id}, {$unset: {'oldfield': "" }}, false, true);
    }
});
```

## Convert ObjectId to String
```
db.posts.find().forEach(function(doc){
     db.accounts.update({_id: doc._id}, {$set: {'_id': doc._id.str }}, false, true);
});
```

## Convert Field Values from Numbers to Strings
```
var newvalue = "";
db.posts.find().forEach(function(doc){
     if(doc.foo){
         newvalue = '"' + doc.foo + '"';
         db.accounts.update({_id: doc._id}, {$set: {'doc.foo': newvalue}});
     }
});
```

## Convert Field Values from Strings to Numbers
```
var newvalue = null;
db.posts.find().forEach(function(doc){
     if(doc.foo){
         newvalue = '"' + doc.foo + '"';
         db.accounts.update({_id: doc._id}, {$set: {'doc.foo': newvalue}});
     }
});
```

## Create a Timestamp from an ObjectID in the _id Field
```
db.posts.find().forEach(function(doc){
    if(doc._id){
        db.posts.update({_id: doc._id}, {$set:{ timestamp: new Date(parseInt(doc._id.str.slice(0,8), 16) *1000) }}, false, true);
    }
});
```

## Create an ObjectID from a Date Object
```
var timestamp = Math.floor(new Date(1974, 6, 25).getTime() / 1000);
var hex       = ('00000000' + timestamp.toString(16)).substr(-8); // zero padding
var objectId  = new ObjectId(hex + new ObjectId().str.substring(8));
```

## Find All the Records that Have Items in an Array
What we're doing here is referencing the array index using dot notation

```
db.posts.find({"tags.0": {$exists: true }})
```

