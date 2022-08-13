---
title: "Debugging"
slug: "debugging"
draft: false
images: []
weight: 9976
type: docs
toc: true
---

## Running debug-only code
Ember has a static global method called [runInDebug][1] which can run a function meant for debugging.

    Ember.runInDebug(() => {
      // this code only runs in dev mode
    });

In a production build, this method is defined as an empty function (NOP). Uses of this method in Ember itself are stripped from the `ember.prod.js` build.


  [1]: http://emberjs.com/api/#method_runInDebug

## Logging EmberData
The ember data models have a [toJSON][1] method that extracts the relevant data:

    console.log(model.toJSON());

This method uses the [JSONSerializer][2] to create the JSON representation.

If you want to log the data in a more app-specific way, you can use [serialize][3]:

    model.serialize();

which uses the serialization strategy you can define in the store's adapter to create a JSON representation of the model.

All objects in an Ember app, including Ember Data models, inherit from [Ember.CoreObject][4], which has a [toString][5] method that prints this representation:

    <app-name@ember-type:object-name:id>

Explanation:

 - `app-name` is the name of your application
 - `ember-type` is the ember type of the object you are logging (can be controller, route etc.)
 - `object-name` is the name of the object you are logging (name of your model, or controller, or route etc.)
 - id is either a guId create with [Ember.guidFor][6] or, for example, the model's id. 

You can overwrite this value using the method `toStringExtension` in your particular model.

For comparison example, here's how logging an application controller could look:

    <my-awesome-app@controller:application::ember324>

  [1]: http://emberjs.com/api/data/classes/DS.Model.html#method_toJSON
  [2]: http://emberjs.com/api/data/classes/DS.JSONSerializer.html
  [3]: http://emberjs.com/api/data/classes/DS.Model.html#method_serialize
  [4]: http://emberjs.com/api/classes/Ember.CoreObject.html
  [5]: http://emberjs.com/api/classes/Ember.CoreObject.html#method_toString
  [6]: http://emberjs.com/api/#method_guidFor

