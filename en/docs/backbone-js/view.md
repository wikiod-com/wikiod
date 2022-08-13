---
title: "View"
slug: "view"
draft: false
images: []
weight: 9980
type: docs
toc: true
---

## Syntax
- Create: `var View = Backbone.View.extend( { /* properties */ } );`
- Construct: `var myView = new View( /* options */ );`
- `initialize`: method automatically called after construction
- `render`: method used to update `this.el` with new content


## A View Bound to Existing HTML
Assuming this HTML in the page:

    <body>
        <div id="myPage">
        </div>
    </body>

A view can be bound to it with:

    var MyPageView = Backbone.View.extend( {
        "el": "#myPage",
        "template": _.template( "<p>This is my page.</p>" ),

        "initialize": function(){
            this.render();
        },

        "render": function(){
            this.$el.html( this.template() );
        }
    } );

    new MyPageView();

The HTML in the browser will now show:
    
    <body>
        <div id="myPage">
            <p>This is my page.</p>
        </div>
    </body>

## View's initialize function
`initialize` is [called by Backbone](http://backbonejs.org/docs/backbone.html#section-152) right after a View is constructed.

# Optional parameters

The `initialize` function receives any arguments passed to the view's constructor. Commonly, the options hash that is used to pass the view's default options: 

    ['model', 'collection', 'el', 'id', 'attributes', 'className', 'tagName', 'events']

You can add any custom attributes to the options hash, and/or custom parameters.

    var MyView = Backbone.View.extend({
        initialize: function(options, customParam){
            // ensure that the 'options' is a hash to avoid errors if undefined.
            options = options || {};
            this.customAttribute = options.customAttribute;
            this.customParam = customParam;
        },
    });

And constructing the view:

    var view = new MyView({
        model: new Backbone.Model(),
        template: "<p>a template</p>",
        customAttribute: "our custom attribute"
    }, "my custom param");

Note that the all the default view options are automatically added to the view object, so it's unnecessary to do that in the `initialize` function.

# Immediately render pattern

One common pattern for the `initialize` method is to call the `render` method so that any newly constructed View is immediately rendered

This pattern should only be used in instances where constructing the object should immediately render it to the HTML document, bind all of the event listeners, and perform all the other actions associated with placing content in the DOM.

    var MyView = Backbone.View.extend({
        initialize: function() {
            this.render();
        },

        render: function() {
            this.$el.html("<p>I'm running!</p>");
        }
    });

It should be noted, however, that *some* Views should not be immediately rendered until `.render` is called manually (or by some other method).

Another common `initialize` pattern is to add things to the View object that will be needed later:

    var MyView = Backbone.View.extend({
        el: "body",
        template: _.template( "<p>This is <%= name %>'s page</p>" ),

        initialize: function(){
            this.name = "Bill";
            
            this.render();
        },

        render: function(){
            var viewTemplateData = {
                name: this.name
            };

            this.$el.html( this.template( viewTemplateData ) );
        }
    });

The DOM will now contain `<p>This is Bill's page</p>` in the `body`.




