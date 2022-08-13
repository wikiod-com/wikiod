---
title: "Getting started with backbone.js"
slug: "getting-started-with-backbonejs"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Basic Setup
Backbone requires [Underscore][_] and (optionally) [jQuery][jquery] - for DOM manipulation (using Backbone.View) and RESTful persistence.

The quickest way to get up and running with Backbone is to create an `index.html` file with simple script tags in the HTML `<head>`:

    <html>
        <head>
            <script src="https://code.jquery.com/jquery-3.1.0.min.js"></script>
            <script src="https://cdnjs.cloudflare.com/ajax/libs/underscore.js/1.8.3/underscore-min.js"></script>

            <script src="https://cdnjs.cloudflare.com/ajax/libs/backbone.js/1.3.3/backbone-min.js"></script>
        </head>
        <body>
        </body>
    </html>

Backbone is now available for use in the page.

[_]: http://underscorejs.org/
[jquery]: http://jquery.com/

## Example of Using Backbone (Primarily Backbone.Model)
Backbone.js is made up of four separate components: Collections, Models, Routers, and Views. Each of these serve different purposes:

- `Model` - represents a single data object, but adds additional functionalities not provided by native JavaScript objects, such as an event system and a more convenient way to retrieve and send data to a remote server 

- `Collection` - represents a set or "collection" of Models and provides functionalities to manage its models.

- `View` - represents a single part of the user interface; each View wraps an HTML DOM element, and provides structure for working with that element as well as convenience features like simple event binding.

- `Router` - enables a "single page application" by allowing an application to trigger different logic (e.g. show different pages) in response to the URL changes.

# Create your own flavors

Before we look at how to use each of these components, let's first take a quick look at Backbone's class system. To create a new sub-class of a Backbone class, you simply call the `extend` method of the original class, and pass it the instance properties and  (static) class properties as objects:

    const MyModelClass = Backbone.Model.extend({
        instanceMethod: function() { console.log('Instance method!'); },
    }, {
        staticMethod: function() { console.log('Static method!'); },
    });

Just as with any other class system, instance methods can be called on instances (objects) of the class, while static methods are called directly on the class itself (the constructor):

    var myInstance = new MyModelClass();
    
    // Call an instance method on our instance
    myInstance.instanceMethod(); // logs "Instance method!"
    
    // Call a static method on our class
    MyModelClass.staticMethod(); // logs "Static method!"

# Using a class

Now, let's look at a quick example of how you can use each class. We'll start with a `Model` of a book.

    const Book = Backbone.Model.extend({
        idAttribute: 'isbn',
        urlRoot: '/book'
    });

Let's break down what just happened there. First, we created a `Book` subclass of `Model`, and we gave it two instance properties.  

 - `idAttribute` tells Backbone to use the "isbn" attribute of the model as its ID when performing AJAX operations.  
 - `urlRoot`, tells Backbone to look for book data on `www.example.com/book`.

Now let's create an instance of a book, and get its data from the server:

    var huckleberryFinn = new Book({ isbn: '0486403491' });
    huckleberryFinn.fetch({
        // the Backbone way
        success: (model, response, options) => {
           console.log(model.get('name')); // logs "Huckleberry Finn"
        }
    }).done(() => console.log('the jQuery promise way'));

When we created a new `Book` we passed it an object, and Backbone uses this object as the initial "attributes" (the data) of the `Model`. Because Backbone knows the `idAttribute` is `isbn`, it knows that the URL for our new Book is `/book/0486403491`.  When we tell it to `fetch`, Backbone will use jQuery to make an AJAX request for the book's data. `fetch` returns a promise (just like `$.ajax`), which you can use to trigger actions once the fetch has completed.

Attributes can be accessed or modified by using the `get` or `set` methods:

    huckleberryFinn.get('numberOfPages'); // returns 64

    huckleberryFinn.set('numberOfPages', 1); // changes numberOfPages to 1

`Models` also have an event system that you can use to react when things happen to a `Model`. For instance, to log a message whenever the `numberOfPages` changes, you could do:

    huckleberryFinn.on('change:numberOfPages', () => console.log('Page change!'));

For a more detailed introduction to the other Backbone classes, view their individual documentation pages.

## Example showcasing the basic concepts
The following example is an introduction to:

 - [Template compilation using underscore][1]
 - Accessing variables in a template
 - Creating a view
 - Rendering a view
 - Showing a view

---

    <html>
    <head>
        <script src="https://code.jquery.com/jquery-3.1.0.min.js"></script>
        <script src="https://cdnjs.cloudflare.com/ajax/libs/underscore.js/1.8.3/underscore-min.js"></script>
        <script src="https://cdnjs.cloudflare.com/ajax/libs/backbone.js/1.3.3/backbone-min.js"></script>
    </head>
    <body>
    
        <div id="example_container"></div>
    
        <script type="text/template" id="example_template">
            <label><%= example_label %></label>
            <input type="text" id="example_input" />
            <input type="button" id="example_button" value="Search" />
        </script>
        <script type="text/javascript">
            var ExampleView = Backbone.View.extend({
                // Compile the template using underscore
                template: _.template($("#example_template").html()),
                events: {
                    "click #example_button": "onButtonClick"
                },
    
                initialize: function(options) {
                    this.customOption = options.customOption;
                },
    
                render: function() {
                    // Load the compiled HTML into the Backbone "el"
                    this.$el.html(this.template({
                        example_label: "My Search"
                    }));
    
                    return this; // for chaining, a Backbone's standard for render
                },
    
                onButtonClick: function(event) {
                    // Button clicked, you can access the button that 
                    // was clicked with event.currentTarget
                    console.log("Searching for " + $("#example_input").val());
                }
            });
            $(function() {
                //show the view inside the div with id 'example_container'
                var exampleView = new ExampleView({
                    el: $("#example_container"),
                    customOption: 41,
                });
                exampleView.render();
            });
        </script>
    </body>
    </html>


  [1]: http://underscorejs.org/#template

## Hello Web (Basic "Hello World"-type setup)
    <html>
        <head>
            <script src="https://code.jquery.com/jquery-3.1.0.min.js"></script>
            <script src="https://cdnjs.cloudflare.com/ajax/libs/underscore.js/1.8.3/underscore-min.js"></script>
    
            <script src="https://cdnjs.cloudflare.com/ajax/libs/backbone.js/1.3.3/backbone-min.js"></script>
            
            <script>
                $( function(){
                    ( function(){
                        var View = Backbone.View.extend( {
                            "el": "body",
                            "template": _.template( "<p>Hello, Web!</p>" ),
        
                            "initialize": function(){
                                this.render();
                            },
                            "render": function(){
                                this.$el.html( this.template() );
                            }
                        } );
        
                        new View();
                    })()
                } );
            </script>
        </head>
        <body>
        </body>
    </html>

