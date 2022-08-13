---
title: "Getting started with angular-material"
slug: "getting-started-with-angular-material"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation or Setup
# Installing Angular Material

**npm**

    npm install angular-material --save
**bower**

    bower install angular-material --save
**jspm**

    jspm install angular-material

**From Cloud**

[cdnjs][1] |
[jsdelivr][2] |
[googlecdn][3]
----------

# Getting Started (blank shell)

<!-- language: lang-html -->
    <html lang="en">
    <head>
      <meta name="viewport" content="width=device-width, initial-scale=1">
      <!-- Angular Material style sheet -->
      <link rel="stylesheet" href="http://ajax.googleapis.com/ajax/libs/angular_material/1.1.4/angular-material.min.css">
    </head>
    <body ng-app="BlankApp" ng-cloak>
      <!--
        Your HTML content here
      -->  
      
      <!-- Angular Material requires Angular.js Libraries -->
      <script src="http://ajax.googleapis.com/ajax/libs/angularjs/1.5.11/angular.min.js"></script>
      <script src="http://ajax.googleapis.com/ajax/libs/angularjs/1.5.11/angular-animate.min.js"></script>
      <script src="http://ajax.googleapis.com/ajax/libs/angularjs/1.5.11/angular-aria.min.js"></script>
      <script src="http://ajax.googleapis.com/ajax/libs/angularjs/1.5.11/angular-messages.min.js"></script>
    
      <!-- Angular Material Library -->
      <script src="http://ajax.googleapis.com/ajax/libs/angular_material/1.1.4/angular-material.min.js"></script>
      
      <!-- Your application bootstrap  -->
      <script type="text/javascript">    
        /**
         * You must include the dependency on 'ngMaterial' 
         */
        angular.module('BlankApp', ['ngMaterial']);
      </script>
      
    </body>
    </html>


  [1]: https://cdnjs.com/libraries/angular-material
  [2]: https://www.jsdelivr.com/projects/angular.material
  [3]: https://developers.google.com/speed/libraries/#angular-material

## Setting up with the CDN
in the index.html, link the CSS from [Google CDN](https://developers.google.com/speed/libraries/#angular-material)
<!-- language: lang-html -->
    <link rel="stylesheet" href="https://ajax.googleapis.com/ajax/libs/angular_material/1.1.1/angular-material.min.css">

Required dependencies:

 - `angular`
 - `angular-aria`
 - `angular-animate`
 - `angular-messages`

<!-- language: lang-html -->
      <!-- Angular Material requires Angular.js Libraries -->
      <script src="https://ajax.googleapis.com/ajax/libs/angularjs/1.5.5/angular.min.js"></script>
      <script src="https://ajax.googleapis.com/ajax/libs/angularjs/1.5.5/angular-animate.min.js"></script>
      <script src="https://ajax.googleapis.com/ajax/libs/angularjs/1.5.5/angular-aria.min.js"></script>
      <script src="https://ajax.googleapis.com/ajax/libs/angularjs/1.5.5/angular-messages.min.js"></script>

      <!-- Angular Material Library -->
      <script src="https://ajax.googleapis.com/ajax/libs/angular_material/1.1.0/angular-material.min.js"></script>

Link: https://material.angularjs.org/latest/getting-started

**index.html**

<!-- language: lang-html -->
        <!DOCTYPE html>
        <html ng-app="angularMaterial">
          <head>
            <link rel="stylesheet"href="https://ajax.googleapis.com/ajax/libs/angular_material/1.1.1/angular-material.min.css">
              <!-- Angular Material requires Angular.js Libraries -->
              <script src="https://ajax.googleapis.com/ajax/libs/angularjs/1.5.8/angular.min.js"></script>
              <script src="https://ajax.googleapis.com/ajax/libs/angularjs/1.5.8/angular-animate.min.js"></script>
              <script src="https://ajax.googleapis.com/ajax/libs/angularjs/1.5.8/angular-aria.min.js"></script>
              <script src="https://ajax.googleapis.com/ajax/libs/angularjs/1.5.8/angular-messages.min.js"></script>
        
              <!-- Angular Material Library -->
              <script src="https://ajax.googleapis.com/ajax/libs/angular_material/1.1.1/angular-material.min.js"></script>
              <script src="app.js"></script>
            <title>Angular Material</title>
          </head>
          <body ng-controller="MainController">
             <md-content>{{content}}</md-content>
          </body>
        </html>

**app.js**
<!-- language: lang-js -->
    angular.module('angularMaterial', ['ngAnimate', 'ngAria', 'ngMaterial', 'ngMessages'])
           .controller('MainController', function($scope) {
                $scope.content = "Your content goes here.";
            })

## Master (HEAD)
`index.html`:
<!-- language: lang-html -->
    <html ng-app="masterAngularMaterial">
        <head>
            <!-- This is important (meta) -->
            <meta name="viewport" content="width=device-width, initial-scale=1">
            <!-- Angular and other dependencies -->
            <script src="https://ajax.googleapis.com/ajax/libs/angularjs/1.5.11/angular.min.js"></script>
            <script src="https://ajax.googleapis.com/ajax/libs/angularjs/1.5.11/angular-animate.min.js"></script>
            <script src="https://ajax.googleapis.com/ajax/libs/angularjs/1.5.11/angular-aria.min.js"></script>
            <script src="https://ajax.googleapis.com/ajax/libs/angularjs/1.5.11/angular-messages.min.js"></script>
            <!-- Angular Material -->
            <script src="https://cdn.rawgit.com/angular/bower-material/master/angular-material.min.js"></script>
            <link href="https://cdn.rawgit.com/angular/bower-material/master/angular-material.min.css" rel="stylesheet">
            <script src="/path/to/app.js"></script>
        </head>
        <body>
            <md-content ng-controller="SomeController">
                {{content}}
            </md-content>
        </body>
    </html>

`app.js`:
<!-- language: lang-js -->
    angular.module('masterAngularMaterial', ['ngAnimate', 'ngAria', 'ngMaterial', 'ngMessages'])
        .controller('SomeController', function($scope) {
            $scope.content="Your content here.";
        })

**Note that importing from https://raw.githubusercontent.com will show this error:**

>Refused to execute script from 'https://raw.githubusercontent.com/angular/bower-material/master/angular-material.min.js' because its MIME type ('text/plain') is not executable, and strict MIME type checking is enabled.

