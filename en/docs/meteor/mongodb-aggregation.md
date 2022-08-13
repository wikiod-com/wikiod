---
title: "MongoDB Aggregation"
slug: "mongodb-aggregation"
draft: false
images: []
weight: 9981
type: docs
toc: true
---

**Server Aggregation**  

http://stackoverflow.com/questions/18520567/average-aggregation-queries-in-meteor

http://stackoverflow.com/questions/15833488/is-it-possible-to-package-a-real-mongodb-library-for-use-on-the-server-side

**Client Aggregation (Minimongo)**  

https://github.com/utunga/pocketmeteor/tree/master/packages/mongowrapper



## Server Aggregation 
Andrew Mao's solution.
http://stackoverflow.com/questions/18520567/average-aggregation-queries-in-meteor

```
Meteor.publish("someAggregation", function (args) {
    var sub = this;
    // This works for Meteor 0.6.5
    var db = MongoInternals.defaultRemoteCollectionDriver().mongo.db;

    // Your arguments to Mongo's aggregation. Make these however you want.
    var pipeline = [
        { $match: doSomethingWith(args) },
        { $group: {
            _id: whatWeAreGroupingWith(args),
            count: { $sum: 1 }
        }}
    ];

    db.collection("server_collection_name").aggregate(        
        pipeline,
        // Need to wrap the callback so it gets called in a Fiber.
        Meteor.bindEnvironment(
            function(err, result) {
                // Add each of the results to the subscription.
                _.each(result, function(e) {
                    // Generate a random disposable id for aggregated documents
                    sub.added("client_collection_name", Random.id(), {
                        key: e._id.somethingOfInterest,                        
                        count: e.count
                    });
                });
                sub.ready();
            },
            function(error) {
                Meteor._debug( "Error doing aggregation: " + error);
            }
        )
    );
});
```

## Aggregation in a Server Method
Another way of doing aggregations is by using the `Mongo.Collection#rawCollection()`

This can only be run on the Server.

Here is an example you can use in Meteor 1.3 and higher:


    Meteor.methods({
       'aggregateUsers'(someId) {
          const collection = MyCollection.rawCollection()
          const aggregate = Meteor.wrapAsync(collection.aggregate, collection)
    
          const match = { age: { $gte: 25 } }
          const group = { _id:'$age', totalUsers: { $sum: 1 } }
    
          const results = aggregate([
             { $match: match },
             { $group: group }
          ])
    
          return results
       }
    })



