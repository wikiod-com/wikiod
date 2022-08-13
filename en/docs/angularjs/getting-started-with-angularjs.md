---
title: "Getting started with AngularJS"
slug: "getting-started-with-angularjs"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Getting Started
Create a new HTML file and paste the following content:

    <!DOCTYPE html>
    <html ng-app>
    <head>
      <title>Hello, Angular</title>
      <script src="https://code.angularjs.org/1.5.8/angular.min.js"></script>
    </head>
    <body ng-init="name='World'">
      <label>Name</label>
      <input ng-model="name" />
      <span>Hello, {{ name }}!</span>
      <p ng-bind="name"></p>
    </body>
    </html>

[Live demo](http://jsfiddle.net/U3pVM/26397/)

When you open the file with a browser, you will see an input field followed by the text `Hello, World!`. Editing the value in the input will update the text in real-time, without the need to refresh the whole page.

---

Explanation:

1. Load the Angular framework from a Content Delivery Network.

       <script src="https://code.angularjs.org/1.5.8/angular.min.js"></script>

2. Define the HTML document as an Angular application with the `ng-app` directive

       <html ng-app>

3. Initialize the `name` variable using `ng-init`

       <body ng-init=" name = 'World' ">

   _Note that ng-init should be used for demonstrative and testing purposes only. When building an actual application, controllers should initialize the data._

4. Bind data from the model to the view on HTML controls. Bind an `<input>` to the `name` property with `ng-model`

       <input ng-model="name" />

5. Display  content from the model using double braces `{{ }}` 

       <span>Hello, {{ name }}</span>

6. Another way of binding the `name` property is using `ng-bind` instead of handlebars`"{{ }}"`  

        <span ng-bind="name"></span>

The last three steps establish the [_two way data-binding_][1]. Changes made to the input update the _model_, which is reflected in the _view_.  

There is a difference between using handlebars and `ng-bind`. If you use handlebars, you might see the actual `Hello, {{name}}` as the page loads before the expression is resolved (before the data is loaded) whereas if you use `ng-bind`, it will only show the data when the name is resolved. As an alternative the directive `ng-cloak` can be used to prevent handlebars to display before it is compiled.


  [1]: https://docs.angularjs.org/guide/databinding

## Showcasing all common Angular constructs
The following example shows common AngularJS constructs in one file:

    <!DOCTYPE html>
    <html ng-app="myDemoApp">
      <head>
        <style>.started { background: gold; }</style>
        <script src="https://code.angularjs.org/1.5.8/angular.min.js"></script>
        <script>
          function MyDataService() {
            return {
              getWorlds: function getWorlds() {
                return ["this world", "another world"];
              }
            };
          }

          function DemoController(worldsService) {
            var vm = this;
            vm.messages = worldsService.getWorlds().map(function(w) {
              return "Hello, " + w + "!";
            });
          }

          function startup($rootScope, $window) {
            $window.alert("Hello, user! Loading worlds...");
            $rootScope.hasStarted = true;
          }
          
          angular.module("myDemoApp", [/* module dependencies go here */])
            .service("worldsService", [MyDataService])
            .controller("demoController", ["worldsService", DemoController])
            .config(function() {
              console.log('configuring application');
            })
            .run(["$rootScope", "$window", startup]);
        </script>
      </head>
      <body ng-class="{ 'started': hasStarted }" ng-cloak>
        <div ng-controller="demoController as vm">
          <ul>
            <li ng-repeat="msg in vm.messages">{{ msg }}</li>
          </ul>
        </div>
      </body>
    </html>

Every line of the file is explained below:

[Live Demo][1]

1. `ng-app="myDemoApp"`, [the ngApp directive][2] that bootstraps the application and tells angular that a DOM element is controlled by a specific `angular.module` named `"myDemoApp"`;
2. `<script src="angular.min.js">` is the first step in [bootstrapping the AngularJS library][3];

Three functions (`MyDataService`, `DemoController`, and `startup`) are declared, which are used (and explained) below.

3. [`angular.module(...)`][4] used with an array as the second argument creates a new module. This array is used to supply a list of module dependencies. In this example we chain calls on the result of the `module(...)` function;
4. `.service(...)` creates an [Angular Service][5] and returns the module for chaining;
5. `.controller(...)` creates an [Angular Controller][6] and returns the module for chaining;
6. `.config(...)` Use this method to register work which needs to be performed on module loading.
7. `.run(...)` makes sure code is [run at startup time][7] and takes an array of items as a parameter. Use this method to register work which should be performed when the injector is done loading all modules.
    - the first item is letting Angular know that the `startup` function requires [the built-in `$rootScope` service][8] to be injected as an argument;
    - the second item is letting Angular know that the `startup` function requires [the built-in `$window` service][9] to be injected as an argument;
    - the *last* item in the array, `startup`, is the actual function to run on startup;

8. `ng-class` is [the ngClass directive][10] to set a dynamic `class`, and in this example utilizes `hasStarted` on the `$rootScope` dynamically
9. `ng-cloak` is [a directive][11] to prevent the unrendered Angular html template (e.g. "`{{ msg }}`") to be briefly shown before Angular has fully loaded the application.
10. `ng-controller` is [the directive][12] that asks Angular to instantiate a new controller of specific name to orchestrate that part of the DOM;
11. `ng-repeat` is [the directive][13] to make Angular iterate over a collection and clone a DOM template for each item;
12. `{{ msg }}` showcases [interpolation][14]: on-the-spot rendering of a part of the scope or controller;


  [1]: https://jsfiddle.net/15vspt5t/
  [2]: https://docs.angularjs.org/api/ng/directive/ngApp
  [3]: https://docs.angularjs.org/guide/bootstrap#angular-script-tag
  [4]: https://docs.angularjs.org/api/ng/function/angular.module
  [5]: https://docs.angularjs.org/guide/services
  [6]: https://docs.angularjs.org/guide/controller
  [7]: http://stackoverflow.com/q/19276095/419956
  [8]: https://docs.angularjs.org/api/ng/service/$rootScope
  [9]: https://docs.angularjs.org/api/ng/service/$window
  [10]: https://docs.angularjs.org/api/ng/directive/ngClass
  [11]: https://docs.angularjs.org/api/ng/directive/ngCloak
  [12]: https://docs.angularjs.org/api/ng/directive/ngController
  [13]: https://docs.angularjs.org/api/ng/directive/ngRepeat
  [14]: https://docs.angularjs.org/guide/interpolation

## The importance of scope
As Angular uses HTML to extend a web page and plain Javascript to add logic, it makes it easy to create a web page using **[ng-app][1]**, **[ng-controller][2]** and some built-in directives such as **[ng-if][3]**, **[ng-repeat][4]**, etc. With the new **controllerAs** syntax, newcomers to Angular users can attach functions and data to their controller instead of using `$scope`.

However, sooner or later, it is important to understand what exactly this `$scope` thing is. It will keep showing up in examples so it is important to have some understanding.

The good news is that it is a simple yet powerful concept.

When you create the following:

    <div ng-app="myApp">
     <h1>Hello {{ name }}</h1>
    </div>

Where does **name** live?

The answer is that Angular creates a `$rootScope` object. This is simply a regular Javascript object and so **name** is a property on the `$rootScope` object:

    angular.module("myApp", [])
      .run(function($rootScope) {
        $rootScope.name = "World!";
      });

And just as with global scope in Javascript, it's usually not such a good idea to add items to the global scope or `$rootScope`.

Of course, most of the time, we create a controller and put our required functionality into that controller. But when we create a controller, Angular does it's magic and creates a `$scope` object for that controller. This is sometimes referred to as the **local scope**.

So, creating the following controller:

    <div ng-app="myApp">
      <div ng-controller="MyController">
        <h1>Hello {{ name }}</h1>
      </div>
    </div>
would allow the local scope to be accessible via the `$scope` parameter.

    angular.module("myApp", [])
      .controller("MyController", function($scope) {
        $scope.name = "Mr Local!";
      });

A controller without a `$scope` parameter may simply not need it for some reason. But it is important to realize that, **even with controllerAs syntax**, the local scope exists.

As `$scope` is a JavaScript object, Angular magically sets it up to prototypically inherit from `$rootScope`. And as you can imagine, there can be a chain of scopes. For example, you could create a model in a parent controller and attach to it to the parent controller's scope as `$scope.model`.

Then via the prototype chain, a child controller could access that same model locally with `$scope.model`.

None of this is initially evident, as it's just Angular doing its magic in the background. But understanding `$scope` is an important step in getting to know how Angular works.

  


  [1]: https://docs.angularjs.org/api/ng/directive/ngApp
  [2]: https://docs.angularjs.org/api/ng/directive/ngController
  [3]: https://docs.angularjs.org/api/ng/directive/ngIf
  [4]: https://docs.angularjs.org/api/ng/directive/ngRepeat

## Minification in Angular
 

   **What is Minification ?**

It is the process of removing all unnecessary characters from source code without changing its functionality.

**Normal Syntax**

If we use normal angular syntax for writing a controller then after minifiying our files it going to break our functionality.

Controller (Before minification) :

    var app = angular.module('mainApp', []);    
    app.controller('FirstController', function($scope) {
        $scope.name= 'Hello World !';  
    });

After using minification tool, It will be minified as like below.

    var app=angular.module("mainApp",[]);app.controller("FirstController",function(e){e.name= 'Hello World !'})

Here, minification removed unnecessary spaces and the $scope variable from code.
So when we use this minified code then its not going to print anything on view. Because $scope is a crucial part between controller and view, which is now replaced by the small 'e' variable. So when you run the application it is going to give Unknown Provider 'e' dependency error.

There are two ways of annotating your code with service name information which are minification safe:

**Inline Annotation Syntax**

    var app = angular.module('mainApp', []);    
    app.controller('FirstController', ['$scope', function($scope) {
        $scope.message = 'Hello World !'; 
    }]);

**$inject Property Annotation Syntax**

    FirstController.$inject = ['$scope'];
    var FirstController = function($scope) {
        $scope.message = 'Hello World !'; 
    }

    var app = angular.module('mainApp', []);
    app.controller('FirstController', FirstController);

After minification, this code will be 

    var app=angular.module("mainApp",[]);app.controller("FirstController",["$scope",function(a){a.message="Hello World !"}]);

Here, angular will consider variable 'a' to be treated as $scope, and It will display output as 'Hello World !'.

## AngularJS Getting Started Video Tutorials
There are a lot of good video tutorials for the AngularJS framework on [egghead.io][1]

[![enter image description here][2]][2]

- https://egghead.io/courses/angularjs-app-from-scratch-getting-started
- https://egghead.io/courses/angularjs-application-architecture
- https://egghead.io/courses/angular-material-introduction
- https://egghead.io/courses/building-an-angular-1-x-ionic-application
- https://egghead.io/courses/angular-and-webpack-for-modular-applications
- https://egghead.io/courses/angularjs-authentication-with-jwt
- https://egghead.io/courses/angularjs-data-modeling
- https://egghead.io/courses/angular-automation-with-gulp
- https://egghead.io/courses/learn-protractor-testing-for-angularjs
- https://egghead.io/courses/ionic-quickstart-for-windows
- https://egghead.io/courses/build-angular-1-x-apps-with-redux
- https://egghead.io/courses/using-angular-2-patterns-in-angular-1-x-apps


  [1]: https://egghead.io
  [2]: https://i.stack.imgur.com/JxQ0P.png

## The Simplest Possible Angular Hello World.
Angular 1 is at heart a DOM compiler. We can pass it HTML, either as a template or just as a regular web page, and then have it compile an app.

We can tell Angular to treat a region of the page as an *expression* using the `{{ }}` handlebars style syntax. Anything between the curly braces will be compiled, like so:

    {{ 'Hello' + 'World' }}

This will output:

    HelloWorld


## **ng-app**

We tell Angular which portion of our DOM to treat as the master template using the `ng-app` *directive*. A directive is a custom attribute or element that the Angular template compiler knows how to deal with. Let's add an ng-app directive now:


    <html>
      <head>
        <script src="/angular.js"></script>
      </head>
      <body ng-app>
        {{ 'Hello' + 'World' }}
      </body>
    </html>

I've now told the body element to be the root template. Anything in it will be compiled.

## **Directives**

Directives are compiler directives. They extend the capabilities of the Angular DOM compiler. This is why **Misko**, the creator of Angular, describes Angular as:

> "What a web browser would have been had it been built for web applications.

We literally create new HTML attributes and elements, and have Angular compile them into an app. `ng-app` is a directive that simply turns on the compiler. Other directives include:

* `ng-click`, which adds a click handler, 
* `ng-hide`, which conditionally hides an element, and 
* `<form>`, which adds additional behaviour to a standard HTML form element.

Angular comes with around 100 built-in directives which allow you to accomplish most common tasks. We can also write our own, and these will be treated in the same way as the built in directives. 

We build an Angular app out of a series of directives, wired together with HTML.

