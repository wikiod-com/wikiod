---
title: "jQuery Deferred objects and Promises"
slug: "jquery-deferred-objects-and-promises"
draft: false
images: []
weight: 9964
type: docs
toc: true
---

jQuery promises are a clever way of chaining together asynchronous operations in a building-block manner. This replaces old-school nesting of callbacks, which are not so easily reorganised.

## jQuery ajax() success, error  VS  .done(), .fail()
**success and Error :**
A **success** callback that gets invoked upon successful completion of an Ajax request.

A **failure** callback that gets invoked in case there is any error while making the request.

**Example:**

     $.ajax({
            url: 'URL',
            type: 'POST',
            data: yourData,
            datatype: 'json',
            success: function (data) { successFunction(data); },
            error: function (jqXHR, textStatus, errorThrown) { errorFunction(); }
        });

**.done() and .fail() :**

.ajax().done(function(data, textStatus, jqXHR){});
Replaces method .success() which was deprecated in jQuery 1.8.This is an alternative construct for the success callback function above. 

.ajax().fail(function(jqXHR, textStatus, errorThrown){});
Replaces method .error() which was deprecated in jQuery 1.8.This is an alternative construct for the complete callback function above.

**Example:** 

    $.ajax({
        url: 'URL',
        type: 'POST',
        data: yourData,
        datatype: 'json'
    })
    .done(function (data) { successFunction(data); })
    .fail(function (jqXHR, textStatus, errorThrown) { serrorFunction(); });

## Basic promise creation
Here is a very simple example of a function that "*promises* to proceed when a given time elapses". It does that by creating a new `Deferred` object, that is resolved later and returning the Deferred's promise:

    function waitPromise(milliseconds){
    
       // Create a new Deferred object using the jQuery static method
       var def = $.Deferred();
    
       // Do some asynchronous work - in this case a simple timer 
       setTimeout(function(){

           // Work completed... resolve the deferred, so it's promise will proceed
           def.resolve();
       }, milliseconds);
    
       // Immediately return a "promise to proceed when the wait time ends"
       return def.promise();
    }

And use like this:

    waitPromise(2000).then(function(){
         console.log("I have waited long enough");
    });


## Get the current state of a promise
By default the state of a promise is pending when it is created. The state of a promise is changed when the deferred object which created the promise either resolves/rejects it.

    var deferred = new $.Deferred();
    var d1= deferred.promise({
        prop: "value"
    });
    var d2= $("div").promise();
    var d3= $("div").hide(1000).promise();
    
    console.log(d1.state()); // "pending"
    console.log(d2.state()); // "resolved"
    console.log(d3.state()); // "pending"

## Asynchronous Promises Chaining
If you have multiple asynchronous tasks that needs to occur one after the other, you will need to chain together their promise objects. Here is a simple example:

    function First() {
        console.log("Calling Function First");
        return $.get("/ajax/GetFunction/First");
    }

    function Second() {
        console.log("Calling Function Second");
        return $.get("/ajax/GetFunction/Second");
    }
     
    function Third() {
        console.log("Calling Function Third");
        return $.get("/ajax/GetFunction/Third");
    }

    function log(results){
        console.log("Result from previous AJAX call: " + results.data);
    }
     
    First().done(log)
           .then(Second).done(log)
           .then(Third).done(log);
