---
title: "Mongo Collections"
slug: "mongo-collections"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

A useful way to think about Mongo collections is in terms of Who, What, When, Where, Why, and How.  Mongo has the following optimizations for different types of data:

**Where** - GeoJSON  
**When** - ObjectID timestamps  
**Who** - Meteor Account Strings  
**How** - JSON for decision trees  

Which leaves the default document in Mongo roughly representing a 'What'.  

## Creating Records in a Legacy Database
You can default to the normal Mongo format by defining your collections with the idGeneration field.

```
MyCollection = new Meteor.Collection('mycollection', {idGeneration : 'MONGO'});
```

## Inserting data into a document
Many beginners to Mongo struggle with basics, such as how to insert an array, date, boolean, session variable, and so forth into a document record.  This example provides some guidance on basic data inputs.

```
Todos.insert({
  text: "foo",                        // String
  listId: Session.get('list_id'),     // String
  value: parseInt(2),                 // Number
  done: false,                        // Boolean
  createdAt: new Date(),              // Dimestamp
  timestamp: (new Date()).getTime(),  // Time
  tags: []                            // Array
});
```

## Getting the _id of the most recently created document
You can get it either synchronously:
```
var docId = Todos.insert({text: 'foo'});
console.log(docId);
```

Or asynchronously:

```
Todos.insert({text: 'foo'}, function(error, docId){
  console.log(docId);
});
```

## Timeseries Data
Using MongoDB for time series data is a very well document and established use-case, with official whitepapers and presentations.  Read and watch the official documentation from MongoDB before trying to invent your own schemas for time series data.  

[MongoDB for Time Series Data](http://www.mongodb.com/presentations/mongodb-time-series-data) 

In general, you'll want to create "buckets" for your timeseries data:

```
DailyStats.insert({
   "date" : moment().format("MM-DD-YYYY"),
   "dateIncrement" : moment().format("YYYYMMDD"),
   "dailyTotal" : 0,
   'bucketA': 0,
   'bucketB': 0,
   'bucketC': 0
   });
```

And then increment those buckets as data feeds into your application.  This increment can be put in a Meteor Method, a collection observer, a REST API endpoint, and various other places.

```
DailyStats.update({_id: doc._id}, {$inc: {bucketA: 1} });
```

For a more complete Meteor example, see the examples from the Clinical Meteor track:

[Realtime Analytics Pipeline](https://github.com/awatson1978/realtime-analytics-pipeline)  
[Clinical Meteor - Graphs - Dailystats](https://github.com/clinical-meteor/graphs-dailystats)

## Filtering with Regexes
Simple pattern for filtering subscriptions on the server, using regexes, reactive session variables, and deps autoruns.

```
// create our collection
WordList =  new Meteor.Collection("wordlist");

// and a default session variable to hold the value we're searching for
Session.setDefault('dictionary_search', '');

Meteor.isClient(function(){
    // we create a reactive context that will rerun items when a Session variable gets updated 
    Deps.autorun(function(){
        // and create a subscription that will get re-subscribe to when Session variable gets updated
        Meteor.subscribe('wordlist', Session.get('dictionary_search'));
    });

    Template.dictionaryIndexTemplate.events({
        'keyup #dictionarySearchInput': function(evt,tmpl){
            // we set the Session variable with the value of our input when it changes
            Session.set('dictionary_search', $('#dictionarySearchInput').val());
        },
        'click #dictionarySearchInput':function(){
            // and clear the session variable when we enter the input
            Session.set('dictionary_search', '');
        },
    });
});
Meteor.isServer(function(){
    Meteor.publish('wordlist', function (word_search) {
        // this query gets rerun whenever the client subscribes to this publication
        return WordList.find({
            // and here we do our regex search
            Word: { $regex: word_search, $options: 'i' }
        },{limit: 100});
    });
});
```

And the HTML that is used on the client:
```
<input id="dictionarySearchInput" type="text" placeholder="Filter..." value="hello"></input>
```

This pattern itself is pretty straight forward, but the regexes may not be. If you're not familiar with regexes, here are some useful tutorials and links:


[Regular Expression Tutorial](http://www.regular-expressions.info/tutorial.html)  
[Regular Expression Cheat Sheet](http://www.cheatography.com/davechild/cheat-sheets/regular-expressions/)  
[Regular Expressions in Javascript](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions)  

## Geospatial Collections - Learning More
Geospatial collections generally involve storing GeoJSON in the Mongo database, streaming that data to the client, accessing the browser's `window.navigator.geolocation`, loading up a Map API, converting GeoJSON to LatLngs, and plotting on the map.  Preferably all in realtime.  Here are a list of resources to get you started:

- [mongodb optimally stores it's data in geoJSON](http://docs.mongodb.org/manual/applications/geospatial-indexes/#geo-overview-location-data)  
- [geojson.org](http://geojson.org/geojson-spec.html)  
- [window.navigator.geolocation](http://www.w3schools.com/jsref/prop_nav_geolocation.asp)  
- [HTML geolocation](http://www.w3schools.com/html/html5_geolocation.asp)  
- [Maps API picker](https://developers.google.com/maps/documentation/api-picker)  
- [google.maps.LatLng](https://developers.google.com/maps/documentation/javascript/examples/map-geolocation)  
- [Google map.data.loadGeoJson](https://developers.google.com/maps/documentation/javascript/examples/layer-data-simple)  
- [meteor-cordova-geolocation-background](https://github.com/zeroasterisk/meteor-cordova-geolocation-background)  
- [phonegap-googlemaps-plugin](https://github.com/wf9a5m75/phonegap-googlemaps-plugin)  
- [LatLng](https://developers.google.com/maps/documentation/business/mobile/android/reference/com/google/android/m4b/maps/model/LatLng)  
- [maps.documentation](https://developers.google.com/maps/documentation/javascript/reference)  
- [google.maps.LatLng](https://developers.google.com/maps/documentation/javascript/reference#LatLng)  
- [2dsphere indexes](http://docs.mongodb.org/manual/core/2dsphere/)  
- [create a 2dsphere index](http://docs.mongodb.org/manual/tutorial/build-a-2dsphere-index/)  
- [query a 2dsphere index](http://docs.mongodb.org/manual/tutorial/query-a-2dsphere-index/)  
- [geospatial indexes and queries](http://docs.mongodb.org/manual/applications/geospatial-indexes/)


## Auditing Collection Queries
The following example will log all of your collection queries to the server console in realtime.

```
Meteor.startup(     
  function () {     
    var wrappedFind = Meteor.Collection.prototype.find;     

    // console.log('[startup] wrapping Collection.find')        

    Meteor.Collection.prototype.find = function () {        
      // console.log(this._name + '.find', JSON.stringify(arguments))       
      return wrappedFind.apply(this, arguments);        
    }       
  },        

  function () {     
    var wrappedUpdate = Meteor.Collection.prototype.update;     

    // console.log('[startup] wrapping Collection.find')        

    Meteor.Collection.prototype.update = function () {      
      console.log(this._name + '.update', JSON.stringify(arguments))        
      return wrappedUpdate.apply(this, arguments);      
    }       
  }     
);      
```

## Observers & Worker Functions
If the Node event loop acts like a bicycle chain, the server-side collection observer is like a derailleur. It's a gearing mechanism that is going to sit on the data collection as the data comes in. It can be very performant, as all race bicycles have derailleurs. But it's also a source for breaking the whole system. It's a high speed reactive function, which can blow up on you. Be warned.

```
Meteor.startup(function(){
  console.log('starting worker....');

  var dataCursor = Posts.find({viewsCount: {$exists: true}},{limit:20});

  var handle = dataCursor.observeChanges({
    added: function (id, record) {
      if(record.viewsCount > 10){
         // run some statistics
         calculateStatistics();

         // or update a value
         Posts.update({_id: id}, {$set:{
           popular: true
         }});

      }
    },
    removed: function () {
      console.log("Lost one.");
    }
  });
});
```

Note the limit of 20 is the size of the derailleur.... how many teeth it has; or, more specifically, how many items are in the cursor as it's walking over the collection. Be careful about using the 'var' keyword in this kind of function. Write as few objects to memory as possibly, and focus on object reuse inside the added method. When the opslog is turned on, and this thing is going full speed, it's a prime candidate for exposing nasty memory leaks if it's writing down objects onto the memory heap faster than the Node garbage collector is able to clean things up.

The above solution won't scale horizontally well, because each Meteor instance will be trying to update the same record. So, some sort of environment detection is necessary for this to scale horizontally.

See the ``percolatestudios:synced-cron`` package for an excellent example of synchronizing service workers across multiple machines in a cluster.  
[meteor-synced-cron](https://github.com/percolatestudio/meteor-synced-cron)

