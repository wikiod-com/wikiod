---
title: "Angular MVC"
slug: "angular-mvc"
draft: false
images: []
weight: 9981
type: docs
toc: true
---

In **AngularJS** the **MVC** pattern is implemented in JavaScript and HTML. The view is defined in HTML, while the model and controller are implemented in JavaScript. There are several ways that these components can be put together in AngularJS but the simplest form starts with the view.


## The Static View with controller
<html ng-app="myApp">
<head>
<title></title>
<meta charset="utf-8" />
<script src="https://ajax.googleapis.com/ajax/libs/angularjs/1.4.5/angular.min.js"></script>
<script src="scripts/ngmyApp.js"></script>
</head>
<body ng-controller="indexController">
<h1>mvc demo</h1>
<div ng-view>
<div id="messageTitle"></div>
<div id="message">Hello World</div>
</div>
</body>
</html>

## Controller Function Definition
    var indexController = myApp.controller("indexController", function ($scope) {
        // Application logic goes here
    });

## Adding information to the model
    var indexController = myApp.controller("indexController", function ($scope) {
        // controller logic goes here
        $scope.message = "Hello Hacking World"
    });

