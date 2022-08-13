---
title: "Sync"
slug: "sync"
draft: false
images: []
weight: 9994
type: docs
toc: true
---

`sync` is a function that Backbone uses to handle all sending or receiving of data to/from a remote server.  The default implementation uses jQuery (or Zepto) to perform AJAX operations when data is synced.  However, this method can be overriden to apply different syncing behavior, such as:

- Using `setTimeout` to batch multiple updates into a single request
- Sending model data as XML instead of JSON
- Using WebSockets instead of Ajax



## Syntax
 - sync(method, model, options)

## Parameters
| parameter | details |
| ------ | ------ |
| method   | create , read , update , delete   |
| model | the model to be saved (or collection to be read)|
|options| success and error callbacks, and all other jQuery request options|



## Basic Example
 The sync() method reads and fetched the model data    


         Backbone.sync = function(method, model) {
            document.write("The state of the model is:");
            document.write("<br>");

            //The 'method' specifies state of the model
            document.write(method + ": " + JSON.stringify(model));
         };

         //'myval' is a collection instance and contains the values which are to be fetched in the collection
         var myval = new Backbone.Collection({
            site:"mrfarhad.ir",
            title:"Farhad Mehryari Official Website"
         });

         //The myval.fetch() method displays the model's state by delegating to sync() method
         myval.fetch();

this code will outputs :

    The state of the model is:
    read: [{"site":"mrfarhad.ir","title":"Farhad Mehryari Official Website"}]

