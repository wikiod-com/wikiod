---
title: "Collection"
slug: "collection"
draft: false
images: []
weight: 9994
type: docs
toc: true
---

## Syntax
 - // New custom collection  
var MyCollection = Backbone.Collection.extend(properties, [classProperties]);
 - // New collection instance  
var collection = new Backbone.Collection([models], [options]);

## Parameters
| Parameter | Details |
| ------ | ------ |
| properties | Instance properties.   |
| classProperties | _Optional._ Properties that exist and are shared with every collection instance of this type.   |
| models | _Optional._ The initial array of models (or objects). If this parameter is left out, the collection will be empty. |
| options | _Optional._ Object which serves to configure the collection and is then passed to the `initialize` function.  |

Collections are ordered sets of models. You can bind `"change"` events to be notified when any model in the collection has been modified, listen for `"add"` and `"remove"` events, `fetch` the collection from the server, and use a full suite of [Underscore.js methods][1].

Any event that is triggered on a model in a collection will also be triggered on the collection directly, for convenience. This allows you to listen for changes to specific attributes in any model in a collection.


  [1]: http://backbonejs.org/#Collection-Underscore-Methods

## Create a custom collection
To create a new collection "class":

    var Books = Backbone.Collection.extend({
        // books will be sorted by title
        comparator: "title",

        initialize: function(models, options) {
            options = options || {};

            // (Optional) you can play with the models here
            _.each(models, function(model) {
                // do things with each model
            }, this);

            this.customProperty = options.property;
        },
    });

All the properties are optional and are there only as a demonstration. A `Backbone.Collection` can be used as-is.

Then using it is as simple as:

    var myBookArray = [
        { id: 1, title: "Programming frontend application with backbone" },
        { id: 2, title: "Backbone for dummies" },
    ];


    var myLibrary = new Books(myBookArray, {
        property: "my custom property"
    });


    myLibrary.each(function(book){
        console.log(book.get('title'));
    });

Will output:

> Programming frontend application with backbone  
> Backbone for dummies



## Fetching and rendering data from the server
We need to define a collection with a `url` property. This is the url to an API endpoint which should return a json formatted array.

    var Books = Backbone.Collection.extend({
        url: "/api/book",
        comparator: "title",
    });

Then, within a view, we'll fetch and render asynchronously:

    var LibraryView = Backbone.View.extend({
        // simple underscore template, you could use 
        // whatever you like (mustache, handlebar, etc.)
        template: _.template("<p><%= title %></p>"),

        initialize: function(options) {
            this.collection.fetch({
                context: this,
                success: this.render,
                error: this.onError
            });
        },

        // Remember that "render" should be idempotent.
        render: function() {
            this.$el.empty();
            this.addAll();

            // Always return "this" inside render to chain calls.
            return this;
        },

        addAll: function() {
            this.collection.each(this.addOne, this);
        },

        addOne: function(model) {
            this.$el.append(this.template(model.toJSON()));
        },

        onError: function(collection, response, options) {
            // handle errors however you want
        },
    });

Simplest way to use this view:

    var myLibrary = new LibraryView({
        el: "body",
        collection: new Books(),
    });

## Collection.url()
By default, the `url` property is not defined. Calling `fetch()` (while using the default `Backbone.sync`) will result in a GET request to the results of `url`.

    var Users = Backbone.Collection.extend({
    
      url: '/api/users',
      
      // or
      
      url: function () {
        return '/api/users'
      }
    
    });
    
    var users = new Users();
    users.fetch() // GET http://webroot/api/users

