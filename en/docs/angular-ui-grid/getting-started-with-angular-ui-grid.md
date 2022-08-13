---
title: "Getting started with angular-ui-grid"
slug: "getting-started-with-angular-ui-grid"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation or Setup
**Requirements**:

Supported Browsers:
 - IE9+
 - Chrome
 - Firefox
 - Safari 5+
 - Opera
 - Android 4


AngularJS
 - 1.4.0+

**Getting Started**

Download the source files from [ui-grid github][1] and include them in your project including the dependencies:

    <link rel="styleSheet" href="release/ui-grid.min.css"/>
    <script src="http://ajax.googleapis.com/ajax/libs/angularjs/1.3.16/angular.min.js"></script>
    <script src="/release/ui-grid.min.js"></script>

At this point you should be able to use ui-grid.

  [1]: https://github.com/angular-ui/ui-grid

## Creating a Simple Grid
**Step 1 - Include the uiGrid in your project**

    <link rel="styleSheet" href="release/ui-grid.min.css"/>
    <script src="http://ajax.googleapis.com/ajax/libs/angularjs/1.3.16/angular.min.js"></script>
    <script src="/release/ui-grid.min.js"></script>

**Step 2 - Add uiGrid module as a dependency to your app**

    var app = angular.module("myApp", ["ui-grid"]);

**Step 3 - Data for the grid**
    
    $scope.myData = [
        {
            "firstName": "John",
            "lastName": "Doe"
        },
        {
            "firstName": "Jane",
            "lastName": "Doe"
        }
    ];

**Step 4 - HTML Markup for the Grid**

Use the `ui-grid` directive and pass in your scope property of `myData`.

    <div ng-controller="mainCtrl">
        <div id="grid1" ui-grid="{ data: myData }"></div>
    </div>

This should render a grid with two columns - First Name and Last Name. 

It takes the keys of the first item of data array as column names and converts the camelCase style keys into proper words: "firstName" to "First Name".

[Example on Plnkr][1]


  [1]: http://plnkr.co/edit/nbhg1tb45bYW0BI7WGgD?p=preview

