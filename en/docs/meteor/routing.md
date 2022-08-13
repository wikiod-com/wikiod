---
title: "Routing"
slug: "routing"
draft: false
images: []
weight: 9984
type: docs
toc: true
---

## Routing with Iron Router
**Install [Iron Router][1]**

From the terminal:

    meteor add iron:router

**Basic configuration**

    Router.configure({
        //Any template in your routes will render to the {{> yield}} you put inside your layout template 
        layoutTemplate: 'layout',
        loadingTemplate: 'loading'
    });


**Render without data**

<!-- language: lang-js -->

    //this is equal to home page
    Router.route('/', function (){
        this.render('home')
    });

    Router.route('/some-route', function () {
        this.render('template-name');
    });

**Render with data and parameters**

<!-- language: lang-js -->

    Router.route('/items/:_id', function () {
        this.render('itemPage', {
            data: function() {
                return Items.findOne({_id: this.params._id})
            }
        });
    });

**Render to a secondary yield**

    Router.route('/one-route/route', function() {
        //template 'oneTemplate' has {{> yield 'secondary'}} in HTML
        this.render('oneTemplate');
        
        //this yields to the secondary place
        this.render('anotherTemplate', {
            to: 'secondary'
        });

        //note that you can write a route  for '/one-route' 
        //then another for '/one-route/route' which will function exactly like above.
    });

**Subscribe and wait for data before rendering template**

    Router.route('/waiting-first', {
        waitOn: function() {
            //subscribes to a publication 
            //shows loading template until subscription is ready
            return Meteor.subscribe('somePublication')
        },
    
        action: function() {
            //render like above examples
        }
    });

**Subscribe to multiple publications and wait for data before rendering template** 

    Router.route('/waiting-first', {
        waitOn: function() {
            //subscribes to a publication 
            //shows loading template until subscription is ready
            return [Meteor.subscribe('somePublication1'),Meteor.subscribe('somePublication2')];
        },
    
        action: function() {
            //render like above examples
        }
    });


Guide for Iron Router: http://iron-meteor.github.io/iron-router/


  [1]: https://github.com/iron-meteor/iron-router

## With FlowRouter
[FlowRouter][1] is more modular compared to Iron Router.

<h1>Install FlowRouter</h1>

<!-- language: lang-bash -->
```
meteor add kadira:flow-router
```

<h1>Rendering a template</h1>

In particular, you must manually add a layout rendering package to link with your rendering engine:

 - [Blaze Layout][2] for Blaze: `meteor add kadira:blaze-layout`
 - [React Layout][3] for React: `meteor add kadira:react-layout`

Then you can render through dynamic templating (in the case of Blaze):

<!-- language: lang-spacebars -->
```
<template name="mainLayout">
  {{> Template.dynamic template=area}}
</template>
```

<!-- language: lang-js -->
```
FlowRouter.route('/blog/:postId', {
  action: function (params) {
    BlazeLayout.render("mainLayout", {
      area: "blog"
    });
  }
});
```

<h1>Rendering a template with parameters and/or query</h1>

The parameters are specified on the route, like with Iron Router:

<!-- language: lang-js -->
```
FlowRouter.route("/blog/:catId/:postId", {
  name: "blogPostRoute",
  action: function (params) {
    //...
  }
})
```

But the parameters are not passed as data context to the child template. Instead, the child template must read them:

<!-- language: lang-js -->
```
// url: /blog/travel/france?showcomments=yes
var catId = FlowRouter.getParam("catId"); // returns "travel"
var postId = FlowRouter.getParam("postId"); // returns "france"

var color = FlowRouter.getQueryParam("showcomments"); // returns "yes"
```



  [1]: https://github.com/kadirahq/flow-router
  [2]: https://github.com/kadirahq/blaze-layout
  [3]: https://github.com/kadirahq/meteor-react-layout

