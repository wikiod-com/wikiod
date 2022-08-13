---
title: "Getting started with angular-ui-router"
slug: "getting-started-with-angular-ui-router"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Hello World Example
**STEP 1: Installation**

Before you can use Angular-UI Router you must include AngularJS itself in your project.
For a detailed guide on that see this [documentation][1].

You can download Angular-UI Router either from their [GitHub-Page][2] or from NuGet, NPM, Bower respectively.

After you have included the JS file in your webpage you can inject the `ui.router` module inside your application.
In your script file you should have something like this:
<!-- language: lang-js -->
    var app = angular.module('app', []);

and now we are going to inject Angular-UI Router into our own application like this:
<!-- language: lang-js -->
    var app = angular.module('app', ['ui.router']);

Now Angular-UI Router will be loaded with our application.
The following steps will explain the basics behind Angular-UI Router and will show some of the basic functionality.

---

**STEP 2: Defining simple states**

You can configure the UI-Router inside the Angular `config` function. Use the `$stateProvider` to define your states. In the following example, each state has a url, controller and a template.

    (function() {
      var app = angular.module('app', ['ui.router']);
      
      app.config(['$stateProvider', function($stateProvider) {
          $stateProvider
            .state('home', {
              url: "/home",
              templateUrl: "home.html",
              controller: "homeCtrl"
            })
            .state('kitchen', {
              url: "/kitchen",
              templateUrl: "kitchen.html",
              controller: "kitchenCtrl"
            })
            .state('den', {
              url: "/denCtrl",
              templateUrl: "den.html",
              controller: "denCtrl"
            })
            .state('itemDetail', {
              url: "/items/:itemName",
              templateUrl: "item.html",
              controller: "itemDetailCtrl"
            })

        }])
    })();

 in your HTML, you will need the `ui-view` directive so that the state views can be populated inside.

    <div ui-view></div>

---

**STEP 3: Accessing states**

There are all together 3 ways to access a state that is defined in `$stateProvider`.

**1. Via `ui-sref` directive**

You can access states inside your HTML, by using the `ui-sref` directive

    <li ui-sref-active="active">
        <a ui-sref="kitchen">Go to the Kitchen</a>
    </li>
    <li ui-sref-active="active">
        <a ui-sref="den">Enter the den</a>
    </li>
    <li ui-sref-active="active">
        <a ui-sref="itemDetail({itemName:'keyboard'})">Key Board</a>
    </li>
    
**2. Via `$state` service in the controller**

you can also navigate to other states inside your controller by using the `$state` provided to the controller with the `.go` method.

    .controller(function($scope, $state) {
        // ...
        $scope.navigateTo = function(stateName) {
            $state.go(stateName); // i.e. $state.go('den'); 
        };
    })

**3. Via the url in browser**

Assuming you have a state called `kitchen` defined like this:

    
    $stateProvider
      .state("kitchen", {
        url: "/kitchenUrl",
        ...
      });

Then accessing appdomain/kitchenUrl as the URL in your browser will go to your `kitchen` state, assuming that there are no nested states and `appdomain` is the server that hosts your application.
    
If you are still confused, here is a fully working [Plnkr][3]


  [1]: https://www.wikiod.com/angularjs/getting-started-with-angularjs#Getting Started
  [2]: https://github.com/angular-ui/ui-router
  [3]: http://plnkr.co/edit/y67KK9gZ5NdIMbMrBF6s?p=preview//

## Basic View
index.html

<!-- language: lang-html -->

    <html>
        <head>
            <title>Angular-UI Router Example</title>
            <script type="text/javascript" src="https://cdnjs.cloudflare.com/ajax/libs/angular.js/1.4.9/angular.js"></script>
            <script type="text/javascript" src="https://cdnjs.cloudflare.com/ajax/libs/angular-ui-router/0.3.1/angular-ui-router.js"></script>
            <script type="text/javascript" src="../js/script.js"></script>
        </head>
        <body ui-view="mainView"> <!-- Defining a container for our view -->
        </body>
    </html>

script.js

<!-- language: lang-js -->

    var app = angular.module('app', ['ui.router']);
    app.config(['$stateProvider', function($stateProvider){
        $stateProvider.state('home', {                        // Creating a state called 'home'
            url: '',                                          // An empty URL means that this state will be loaded on the main URL when no other state is called
            views: {
                'mainView': {                                 // Section for our view-container that we defined in the HTML
                    template: '<h1>It works!</h1>'            // Setting a template for this view
                    /*templateUrl: '../templates/home.html'*/ //templateUrl would load the file and uses it's content as the template
                 }
            }
        });
    }])



## Defining a state with multiple view
In ui-router a state can hold multiple views, each with his own controller and a template

    .state('dashboard', {
         name: 'dashboard',
         url: '/dashboard',
         views: {
             "view1": {
                 templateUrl: "path/to/view1.html",
                 controller: "view1Controller"
             },
             "view2": {
                 templateUrl: "path/to/view2.html",
                 controller: "view2Controller"
             }
         }
     })

Then inside your state's HTML, you can link these views

    <div ui-view="view1"></div>
    <div ui-view="view2"></div>

## Resolving data into a state
You can `resolve` data into your state when you transition into it, usually it's useful when the state needs to use that data, or to resolve into a state when some provided input needs to be authenticated.

When you define your states, you will need to provide a map of values to be resolved into the `.resolve` property, each resolved value should have a function that returns a `promise`

    .state('main', {
         url: "/main",
         templateUrl: "path/to/main.html",
         controller: 'mainCtrl',
         resolve: {
             serverData: function ($http) {
                 return $http.get('some/url');
             }
         }
    });

Now, inside the `mainCtrl` you can access the data (that is if the `$http` call resolved successfully).

    .controller("mainCtrl", function($scope, serverData) {
        $scope.resolvedData = serverData.then(resp=> resp.data);
        ....
    })

## Using transition events
UI-Router exposes transition events that can be helpful for handling transition errors, handling/blocking transitions based on certain parameter values, custom authentication etc..

These events can be bound to `$rootScope` for a global effect or to `$scope` for a per controller effect.


----------


`$stateChangeError` - This event is broadcasted when an attempt to change the state has failed and threw and error, this event fires a callback function with the following signature:

>callback(event, toState, toParams, fromState, fromParams, error)

*event*: the event object

*toState*: the target state 

*toParams*: the parameters passed to the target state 

*fromState*: current state 

*fromParams*: the parameters passed to the current state 

*error*: the error object


----------


 `$stateChangeStart` - This event is broadcasted when a state transition started, this event fires a callback function with the following signature:

> callback(event, toState, toParams, fromState, fromParams, options)

*options*: the state options object

`$stateChangeSuccess` - This event is broadcasted when a state transition completes, this event fires a callback function with the following signature:

> callback(event, toState, toParams, fromState, fromParams, options)


----------


`$stateNotFound` - This event is broadcasted when a state you requested to transition to was not found, this event fires a callback function with the following signature:

> callback(event, unfoundState, fromParams, fromState)

*unfoundState* - an object representing the state that was not found


----------

**Example:** 
````
$rootScope.$on('$stateChangeSuccess', function (event, toState, toParams, fromState, fromParams, options) {
    $log.debug("$stateChangeSuccess: event: %o toState: %o, toParams: %o, fromState: %o, fromParams: %o, options: %o", event, toState, toParams, fromState, fromParams, options);
    // runs when the state has successfully changed
});

$rootScope.$on('$stateChangeStart', function (event, toState, toParams, fromState, fromParams, options) {
    $log.debug("$stateChangeStart: event: %o toState: %o, toParams: %o, fromState: %o, fromParams: %o, options: %o", event, toState, toParams, fromState, fromParams, options);
    // runs when the state has just started to transition
});

$rootScope.$on('$stateNotFound', function (event, unfoundState, fromParams, fromState) {
    $log.debug("$stateNotFound: event: %o unfoundState: %o, fromParams: %o, fromState: %o", event, unfoundState, fromParams, fromState);
    // runs when the state wsa not found
});

$rootScope.$on('$stateChangeError', function (event, toState, toParams, fromState, fromParams, error) {
    $log.debug("$stateChangeError: event: %o toState: %o, toParams: %o, fromState: %o, fromParams: %o, error: %o", event, toState, toParams, fromState, fromParams, error);
    // runs when there was an error while attempting to transition
});
````



