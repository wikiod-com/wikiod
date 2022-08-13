---
title: "Retrieving data from a Meteor.call"
slug: "retrieving-data-from-a-meteorcall"
draft: false
images: []
weight: 9961
type: docs
toc: true
---

## The basics of Meteor.call
    Meteor.call(name, [arg1, arg2...], [asyncCallback])

(1) name String \
(2) Name of method to invoke \
(3) arg1, arg2... EJSON-able Object [Optional]\
(4) asyncCallback Function [Optional]

On one hand, you can do : (via **Session** **variable**, or via **ReactiveVar**)

        var syncCall = Meteor.call("mymethod") // Sync call 

It mean if you do something like this, server side you will do : 

        Meteor.methods({
            mymethod: function() {
                let asyncToSync =  Meteor.wrapAsync(asynchronousCall);
                // do something with the result;
                return  asyncToSync; 
            }
        });

On the other hand, sometimes you will want to keep it via the result of the callback ?

**Client side :** 

    Meteor.call("mymethod", argumentObjectorString, function (error, result) {
        if (error) Session.set("result", error); 
        else Session.set("result",result);
    }
    Session.get("result") -> will contain the result or the error;

    //Session variable come with a tracker that trigger whenever a new value is set to the session variable. \ same behavior using ReactiveVar
**Server side**

    Meteor.methods({
        mymethod: function(ObjectorString) {
            if (true) {
                return true;
            } else {
                throw new Meteor.Error("TitleOfError", "ReasonAndMessageOfError"); // This will and up in the error parameter of the Meteor.call
            }
        }
    });

The purpose here is to show that Meteor propose various way to communicate between the Client and the Server. 

## Using Session variable
## Server side

<!-- language: lang-js -->

    Meteor.methods({
      getData() {
        return 'Hello, world!';
      }
    });

## Client side

<!-- language: lang-html -->

    <template name="someData">
      {{#if someData}}
        <p>{{someData}}</p>
      {{else}}
        <p>Loading...</p>
      {{/if}}
    </template>

<!-- language: lang-js -->

    Template.someData.onCreated(function() {
      Meteor.call('getData', function(err, res) {
        Session.set('someData', res);
      });
    });

    Template.someData.helpers({
      someData: function() {
        return Session.get('someData');
      }
    });


## Using ReactiveVar
## Server side

<!-- language: lang-js -->

    Meteor.methods({
      getData() {
        return 'Hello, world!';
      }
    });

## Client side

<!-- language: lang-html -->

    <template name="someData">
      {{#if someData}}
        <p>{{someData}}</p>
      {{else}}
        <p>Loading...</p>
      {{/if}}
    </template>

<!-- language: lang-js -->

    Template.someData.onCreated(function() {

      this.someData = new ReactiveVar();

      Meteor.call('getData', (err, res) => {
        this.someData.set(res);
      });
    });

    Template.someData.helpers({
      someData: function() {
        return Template.instance().someData.get();
      }
    });

`reactive-var` package required. To add it run `meteor add reactive-var`.

