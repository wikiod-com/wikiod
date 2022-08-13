---
title: "Getting started with angularjs-directive"
slug: "getting-started-with-angularjs-directive"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Building a reusable component
Directives can be used to build reusable components. Here is an example of a "user box" component:

**userBox.js**

    angular.module('simpleDirective', []).directive('userBox', function() {
      return {
        scope: {
            username: '=username',
            reputation: '=reputation'
        },
        templateUrl: '/path/to/app/directives/user-box.html'
      };
    });


**Controller.js**

    var myApp = angular.module('myApp', ['simpleDirective']);
    
    myApp.controller('Controller', function($scope) {
        $scope.user = "John Doe";
        $scope.rep = 1250;
    });


**myPage.js**

    <html lang="en" ng-app="myApp">
      <head>
        <script src="/path/to/app/angular.min.js"></script>
        <script src="/path/to/app/controllers/Controller.js"></script>
        <script src="/path/to/app/directives/userBox.js"></script>
      </head>
    
      <body>
      
        <div ng-controller="Controller">
            <user-box username="user" reputation="rep"></user-box>
        </div>
        
      </body>
    </html>


**user-box.html**

    <div>{{username}}</div>
    <div>{{reputation}} reputation</div>



## Your first directive 
Our first element directive will not do much: it will just calculate `2+2` and will be called in html like this: 

    <my-calculator></my-calculator>

Notice the name of the directive is `myCalculator` (in CamelCase), but in html it's used as `my-calculator` (in lisp-case).

Since we want our directive to be used as html element,  we will use `restrict: 'E'`. 

Every directive has the template which will be compiled and inserted. Our directive is very simple, so we will insert our html as string into a `template` parameter.

    // directives/my-calculator.js

    angular.module('exampleApp', [])
    .directive('myCalculator', function() {
      return {
        restrict: 'E',
        template: '<span> My directive can calculate 2+2: {{2+2}} </span>'
      };
    });


**HTML**

    <!DOCTYPE html>
    <html ng-app="exampleApp">
    
      <head>
        <script src="https://ajax.googleapis.com/ajax/libs/angularjs/1.5.6/angular.min.js"></script>
        <script src="my-calculator.js"></script>
      </head>
    
      <body>
        Here is my first directive: 
        <my-calculator></my-calculator>
      </body>
    
    </html>

The result will look like this:
> Here is my first directive:
> My directive can calculate 2+2: 4

If you want to play with the live example, go to [plunkr][1].


  [1]: https://plnkr.co/edit/p9yVSCf7wp8014il4AMb?p=preview

## Installation or Setup
Directives comes with the AngularJS library itself. A sample directive can be created as:

    angular.module('simpleDirective', [])
    .directive('helloData', function() {
      return {
        template: 'Hello, {{data}}'
      };
    });

And can be used as:

**JS:**

    angular.module('app', ['simpleDirective'])
    .controller('Controller', ['$scope', function($scope) {
      $scope.data = 'World';
    }])

**HTML**

    <div ng-controller="Controller">
      <div hello-data></div>
    </div>

Will be compiled as:

> Hello, World



## Success/Error pop-up message using simple  link function
Link function is best way in custom directives to manipulate DOM.
It takes three attributes as input (scope, element, attribute) in sequence 

scope: its local scope object of directive.

element: html element on which directive is used.

attribute: it gives access to all attributes used in element refered.


    // on success call or similarly error, warning, info in controller
        $scope.message={
                    text: "Saved Successfully",
                    type: "SUCCESS"
                    };    
                    
        <user-info msg="message"> </user-info>   //in html


 
        var mainApp = angular.module("mainApp", []);
         mainApp.directive('userInfo', function() {
            var directive = {};
            directive.restrict = 'E';
            
            directive.scope = {
               message : "=msg"
            },
            
            directive.link = function(scope, element, attributes) {
                if(scope.message.type==='SUCCESS')
                  scope.message.text = 'SUCCESS: '+scope.message.text+' !';
                else if(scope.message.type==='ERROR')  
                  scope.message.text = 'ERROR: '+scope.message.text+' !';
                else if(scope.message.type==='WARNING')  
                  scope.message.text = 'WARNING: '+scope.message.text+' !'
                else if(scope.message.type==='INFO')  
                  scope.message.text = 'INFO: '+scope.message.text+' !'
              
                element.on('click', function(event) {     //on click of div pop-up will smoothly close
                          $(this).fadeOut();   
                          });
               },
               directive.template = '<div ng-class={{message.type}}>'+            // one can create different bg-color as per type of message and width/height
                                    '<div class="message-text">{{message.text}}<div>'+   //message text will be printed
                                    '<div>';
               
            return directive;
         });




