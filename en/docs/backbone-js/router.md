---
title: "Router"
slug: "router"
draft: false
images: []
weight: 9991
type: docs
toc: true
---

## Creating a router
The web server serves the user based on the request sent by the browser but how the user will tell the browser what he/she is looking for, that's when we need URL. Every web page on the internet has got a URL that can be bookmarked, copied, shared, and saved for future reference. In single page Backbone app, everything we see is a view, views are formed and rendered to show specific page but what if the user want's to see the same information again. To achieve this we need to implement a Backbone router based app which will render views based on the route name and parameters.

**A very simple example of a Backbone router:**

    var UserList = Backbone.Router.extend({
       routes: {//List of URL routes with the corresponding function name which will get called when user will visit a page having URL containing this route
           "list":                "list",    // localhost:8080/#list
           "search/:name":        "search",  // localhost:8080/#search/saurav
           "search/:name/p:page": "search",  // localhost:8080/#search/kiwis/p7
           "profile/:userId":     "profile" // localhost:8080/#profile/92
       },
       list: function() {
           var userCollection = new UserCollection();
           var userCollectionView = new UserCollectionView();
           userCollection.fetch({remove : true, data:{}, success: function(){
               for(var i = 0; i < userCollection.length; i++ ){
                   var userModel = userCollection.at(i);
                   var userView = new UserView({ model: userModel });
                   userView.render();
                   userCollectionView.$el.append(userView.$el);
               }
            }});
       },
       search: function(name, page) {
           var userCollection = new UserCollection();
           var userCollectionView = new UserCollectionView();
           userCollection.fetch({remove : true, data:{pageNo: page, name: name}, success: function(){
               for(var i = 0; i < userCollection.length; i++ ){
                   var userModel = userCollection.at(i);
                   var userView = new UserView({ model: userModel });
                   userView.render();
                   userCollectionView.$el.append(userView.$el);
               }
            }});
       },
       profile: function(userId){
           var userModel = new UserModel({id: userId});
           userModel.fetch({success: function(){
                 var userView = new UserView({model: userModel});
                 userView.render();
           }});
       }
    });
    var userList = new UserList();
    Backbone.history.start();

The above code is only an example code which demonstrates how you can create a Backbone router and get parameters from URL to render corresponding views.

Explanation of how the above router will work and behave:

**Cases by URLs:**

 - **localhost:8080/#search/saurav** - router's "search" route (function) will get triggered with parameters name = "saurav" and page = null, now userCollection.fetch() function will fetch all the users having name = "saurav" from backend and it will render each user's details one by one.

 - **localhost:8080/#search/saurav/p6** - router's "search" route (function) will get triggered with parameters name = "saurav" and page = 6, now userCollection.fetch() function will fetch all the users having name = "saurav" of page 6 from backend and it will render each user's details one by one.

 - **localhost:8080/#list** - router's "list" route (function) will get triggered, now userCollection.fetch() function will fetch all the users from backend and it will render each user's details one by one.

 - **localhost:8080/#profile/92** - router's "profile" route (function) will get triggered, we will create a new instance of userModel with id = userId i.e. 92 and we will fetch the user's details from backend and render the userView with that data. 

**An easy to experiment example:**

Visit http://backbonejs.org in chrome browser, open the developer tools console and paste the below code-

    var Workspace = Backbone.Router.extend({

        routes: {
           "help":                 "help",    // #help
           "search/:query":        "search",  // #search/kiwis
           "search/:query/p:page": "search"   // #search/kiwis/p7
        },

        help: function() {
           console.log("help");
        },

        search: function(query, page) {
           console.log("searched " + query + " " + page);
        }
    });
    var work = new Workspace();
    Backbone.history.start();


Now, replace the URL in the browser with "http://backbonejs.org/#search/kiwis/p9" and hit the enter key. This will trigger "search" route (function) with parameters query = "kiwis" and page = 9 and you will see an output in the browser console i.e. "searched kiwis 9".

Changing the route through code:

 - **Case 1**: execute code `work.navigate("search/kiwis/p7", {trigger: true});` in the console and it will print output "searched kiwis 7" but if you will try to execute the same code with the same parameter then nothing will happen, see next case.

 - **Case 2**: execute code `work.navigate("search/kiwis/p7", {trigger: false});` in the console it will not print anything because route will not get triggered.

 - **Case 3**: In case if you need to reload the current route once again then you will need to execute this code `Backbone.history.loadUrl("search/kiwis/p7");`.

 - **Case 4**: Executing the code `work.navigate("search/kiwis/p15");` will just change the URL but it will not trigger the corresponding route (function).

 - **Case 5**: Executing the code `work.navigate("search/kiwis/p11", {trigger: true});` will change the URL and trigger the route.

- **Case 6**: Executing code `work.navigate("search/kiwis/p17", {trigger: true, replace: true})` will replace the existing route with this route hence clicking browser's back button will take you 2 routes back to "search/kiwis/p15".



  [1]: http://backbonejs.org/

