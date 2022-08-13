---
title: "AngularJS gotchas and traps"
slug: "angularjs-gotchas-and-traps"
draft: false
images: []
weight: 9861
type: docs
toc: true
---

## Things to do when using html5Mode
When using `html5Mode([mode])` it is necessary that:

 1. You specify the base URL for the application with a `<base href="">` in the head of your `index.html`.
 2. It is important that the `base` tag comes before any tags with url requests. Otherwise, this might result in this error - `"Resource interpreted as stylesheet but transferred with MIME type text/html"`. For example:

        <head>
            <meta charset="utf-8">
            <title>Job Seeker</title>
    
            <base href="/">
    
            <link rel="stylesheet" href="bower_components/bootstrap/dist/css/bootstrap.css" />
            <link rel="stylesheet" href="/styles/main.css">
        </head>

 3. If you do no want to specify a `base` tag, configure `$locationProvider` to not require a `base` tag by passing a definition object with `requireBase:false` to `$locationProvider.html5Mode()` like this:

        $locationProvider.html5Mode({
            enabled: true,
            requireBase: false
        });

4. In order to support direct loading of HTML5 URLs, you need to enabler server-side URL rewriting. From [AngularJS / Developer Guide / Using $location][1]

   > Using this mode requires URL rewriting on server side, basically you have to rewrite all your links to entry point of your application (e.g. `index.html`). Requiring a `<base>` tag is also important for this case, as it allows Angular to differentiate between the part of the url that is the application base and the path that should be handled by the application.

   An excellent resource for request rewriting examples for various HTTP server implementations can be found in the [ui-router FAQ - How to: Configure your server to work with html5Mode][2]. For example, Apache

        RewriteEngine on

        # Don't rewrite files or directories
        RewriteCond %{REQUEST_FILENAME} -f [OR]
        RewriteCond %{REQUEST_FILENAME} -d
        RewriteRule ^ - [L]

        # Rewrite everything else to index.html to allow html5 state links
        RewriteRule ^ index.html [L]

   nginx

        server {
            server_name my-app;
    
            root /path/to/app;
    
            location / {
                try_files $uri $uri/ /index.html;
            }
        }

   Express

        var express = require('express');
        var app = express();

        app.use('/js', express.static(__dirname + '/js'));
        app.use('/dist', express.static(__dirname + '/../dist'));
        app.use('/css', express.static(__dirname + '/css'));
        app.use('/partials', express.static(__dirname + '/partials'));

        app.all('/*', function(req, res, next) {
            // Just send the index.html for other files to support HTML5Mode
            res.sendFile('index.html', { root: __dirname });
        });

        app.listen(3006); //the port you want to use


  [1]: https://docs.angularjs.org/guide/$location#server-side
  [2]: https://github.com/angular-ui/ui-router/wiki/Frequently-Asked-Questions#how-to-configure-your-server-to-work-with-html5mode

## Two-way data binding stops working
One should have in mind that: 
1. Angular's data binding relies on JavaScript’s prototypal inheritance, thus it's subject to [variable shadowing](https://en.wikipedia.org/wiki/Variable_shadowing).
1. A child scope normally prototypically inherits from its parent scope. <sub><sup>One exception to this rule is a directive which has an isolated scope as it doesn't prototypically inherit.</sub></sup>
1. There are some directives which create a new child scope: `ng-repeat`, `ng-switch`, `ng-view`, `ng-if`, `ng-controller`, `ng-include`, etc.

This means that when you try to two-way bind some data to a primitive which is inside of a child scope (or vice-versa), things may not work as expected. [Here](https://jsfiddle.net/4bpbhmtL/)'s an example of how easily is to *"break"* AngularJS.

This issue can easily be avoided following these steps:

 1. Have a "." inside your HTML template whenever you bind some data
 2. Use `controllerAs` syntax as it promotes the use of binding to a "dotted" object
 3. $parent can be used to access parent `scope` variables rather than child scope. like inside `ng-if` we can use `ng-model="$parent.foo"`..

----------

An alternative for the above is to bind `ngModel` to a getter/setter function that will update the cached version of the model when called with arguments, or return it when called without arguments. 
In order to use a getter/setter function, you need to add `ng-model-options="{ getterSetter: true }"` to the element with the `ngModal` attribute, and to call the getter function if you want to display its value in expression ([Working example](https://jsfiddle.net/4bpbhmtL/5/)). 

## Example ##
View:

    <div ng-app="myApp" ng-controller="MainCtrl">
        <input type="text" ng-model="foo" ng-model-options="{ getterSetter: true }">
        <div ng-if="truthyValue">
            <!-- I'm a child scope (inside ng-if), but i'm synced with changes from the outside scope -->
            <input type="text" ng-model="foo">
        </div>
        <div>$scope.foo: {{ foo() }}</div>
    </div>
    
Controller:

    angular.module('myApp', []).controller('MainCtrl', ['$scope', function($scope) {
        $scope.truthyValue = true;
          
        var _foo = 'hello'; // this will be used to cache/represent the value of the 'foo' model 
          
        $scope.foo = function(val) {
            // the function return the the internal '_foo' varibale when called with zero arguments,
            // and update the internal `_foo` when called with an argument
            return arguments.length ? (_foo = val) : _foo;
        };
    }]);

**Best Practice**: It's best to keep getters fast because Angular is likely to call them more frequently than other parts of your code ([reference](https://docs.angularjs.org/api/ng/directive/ngModel)).

## 7 Deadly Sins of AngularJS
Below is the list of some mistakes that developers often make during the use of AngularJS functionalities, some learned lessons and solutions to them.

<strong>1. Manipulating DOM through the controller</strong>

It's legal, but must be avoided. Controllers are the places where you define your dependencies, bind your data to the view and make further business logic. You can technically manipulate the DOM in a controller, but whenever you need same or similar manipulation in another part of your app, another controller will be needed. So the best practice of this approach is creating a directive that includes all manipulations and use the directive throughout your app. Hence, the controller leaves the view intact and does it's job.
In a directive, linking function is the best place to manipulate the DOM. It has full access to the scope and element, so using a directive, you can also take the advantage of reusability.

    link: function($scope, element, attrs) {
        //The best place to manipulate DOM
    }

 You can access DOM elements in linking function through several ways, such as the `element` parameter, `angular.element()` method, or pure Javascript.


<strong>2. Data binding in transclusion</strong>

AngularJS is famous with its two-way data binding. However you may encounter sometimes that your data is only one-way bound inside directives. Stop there, AngularJS is not wrong but probably you. Directives are a little dangerous places since child scopes and isolated scopes are involved. Assume you have the following directive with one transclusion

    <my-dir>
        <my-transclusion>
        </my-transclusion>
    </my-dir>

And inside my-transclusion, you have some elements which are bound to the data in the outer scope.

    <my-dir>
        <my-transclusion>
            <input ng-model="name">
        </my-transclusion>
    </my-dir>

The above code will not work correctly. Here, transclusion creates a child scope and you can get the name variable, right, but whatever change you make to this variable will stay there. So, you can truly acces this variable as <strong>$parent.name</strong>. However, this use might not be the best practice. A better approach would be wrapping the variables inside an object. For example, in the controller you can create:

    $scope.data = {
        name: 'someName'
    }

Then in the transclusion, you can access this variable via 'data' object and see that two-way binding works perfectly!

    <input ng-model="data.name">

Not only in transclusions, but throughout the app, it's a good idea to use the dotted notation.

<strong>3. Multiple directives together</strong>

It is actually legal to use two directives together within the same element, as long as you obey by the rule: two isolated scopes cannot exist on the same element. Generally speaking, when creating a new custom directive, you allocate an isolated scope for easy parameter passing. Assuming that the directives myDirA and myDirB have isoleted scopes and myDirC has not, following element will be valid:

    <input my-dir-a my-dirc>

whereas the following element will cause console error:

    <input my-dir-a my-dir-b>

Therefore, directives must be used wisely, taking the scopes into consideration.

<strong>4. Misuse of $emit</strong>

$emit, $broadcast and $on, these work in a sender-receiver principle. In others words, they are a means of communication between controllers. For example, the following line emits the 'someEvent' from controller A, to be catched by the concerned controller B.

    $scope.$emit('someEvent', args);

And the following line catches the 'someEvent'

    $scope.$on('someEvent', function(){});

So far everything seems perfect. But remember that, if the controller B is not invoked yet, the event will not be caught, which means both emitter and receiver controllers have to be invoked to get this working. So again, if you are not sure that you definitely have to use $emit, building a service seems a better way.

<strong>5. Misuse of $scope.$watch</strong>

$scope.$watch is used for watching a variable change. Whenever a variable has changed, this method is invoked. However, one common mistake done is changing the variable inside $scope.$watch. This will cause inconsistency and infinite $digest loop at some point.

    $scope.$watch('myCtrl.myVariable', function(newVal) {
        this.myVariable++;
    });

So in the above function, make sure you have no operations on myVariable and newVal. 

<strong>6. Binding methods to views</strong>

This is one of the deadlisest sins. AngularJS has two-way binding, and whenever something changes, the views are updated many many times. So, if you bind a method to an attribute of a view, that method might potentially be called a hundred times, which also drives you crazy during debugging. However, there are only some attributes that are built for method binding, such as ng-click, ng-blur, ng-on-change, etc, that expect methods as paremeter.
For instance, assume you have the following view in your markup:

    <input ng-disabled="myCtrl.isDisabled()" ng-model="myCtrl.name">

Here you check the disabled status of the view via the method isDisabled. In the controller myCtrl, you have:

    vm.isDisabled = function(){
        if(someCondition)
            return true;
        else
            return false;
    }

In theory, it may seem correct but technically this will cause an overload, since the method will run countless times. In order to resolve this, you should bind a variable. In your controller, the following variable must exist:

    vm.isDisabled

You can initiate this variable again in the activation of the controller

    if(someCondition)
        vm.isDisabled = true
    else
        vm.isDisabled = false

If the condition is not stable, you may bind this to another event. Then you should bind this variable to the view:

    <input ng-disabled="myCtrl.isDisabled" ng-model="myCtrl.name">

Now, all the attributes of the view have what they expect and the methods will run only whenever needed.

<strong>7. Not using Angular's functionalities</strong>

AngularJS provides great convenience with some of its functionalities, not only simplifying your code but also making it more efficient. Some of these features are listed below:
<ol>
    <li><strong>angular.forEach</strong> for the loops (Caution, you can't "break;" it, you can only prevent getting into the body, so consider performance here.)</li>
    <li><strong>angular.element</strong> for DOM selectors</li>
        <li><strong>angular.copy</strong>: Use this when you should not modify the main object</li>
    <li><strong>Form validations</strong> are already awesome. Use dirty, pristine, touched, valid, required and so on.</li>
    <li>Besides Chrome debugger, use <strong>remote debugging</strong> for mobile development too.</li>
    <li>And make sure you use <strong>Batarang</strong>. It's a free Chrome extension where you can easily inspect scopes</li>
.</ol>





