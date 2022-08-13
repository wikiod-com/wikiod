---
title: "md-button"
slug: "md-button"
draft: false
images: []
weight: 9939
type: docs
toc: true
---

## Types of <md-button>
Make sure you link the Angular and Angular Material libraries!

**index.html**:
<!-- language: lang-html -->
    <html ng-app="mdButtonApp">
        <head>
            <!-- Import Angular -->
            <script src="https://ajax.googleapis.com/ajax/libs/angularjs/1.5.8/angular.min.js"></script>
            <script src="https://ajax.googleapis.com/ajax/libs/angularjs/1.5.8/angular-animate.min.js"></script>
            <script src="https://ajax.googleapis.com/ajax/libs/angularjs/1.5.8/angular-aria.min.js"></script>
            <!-- Angular Material -->
            <script src="https://ajax.googleapis.com/ajax/libs/angular_material/1.5.8/angular-material.min.js"></script>
            <link href="https://ajax.googleapis.com/ajax/libs/angular_material/1.1.1/angular-material.min.css" rel="stylesheet">
            <script src="app.js"></script>
        </head>
        <body>
            <md-content ng-controller="mdButtonController">
                <!-- Normal `md-button` -->
                <md-button>Normal</md-button>
                <md-button class="md-primary">{{text}}</md-button>
                <md-button class="md-accent">{{text}}</md-button>
                <md-button class="md-warn" ng-href="{{link}}">Google</md-button>
                <md-button class="md-raised" ng-click="goToLink('http://example.com')">Link</md-button>
                <md-button class="md-cornered md-primary md-hue-1">{{text}}</md-button>
                <md-button class="md-accent md-hue-2">Some Button</md-button>
                <md-button class="md-warn md-hue-3" ng-href="{{link}}">{{text}}</md-button>
            </md-content>
        </body>
    </html>

**app.js**:
<!-- language: lang-js -->
    angular.module('mdButtonApp', ['ngAnimate', 'ngAria', 'ngMaterial', 'ngMessages'])
    .controller('mdButtonController', function($scope) {
    $scope.text = "Button";
    $scope.link = "https://google.com";
    $scope.goToLink = function(link) {
            // Go to some site
            console.log('Redirecting to:' + link);
            window.location.href=link;
        }
    })

## Creating an icon button
This example will be using the class `md-icon-button`, which must be applied to `<md-button>` in order to get an icon button. 
- It is also recommended to add an `aria-label` attribute to `<md-button>` for accessibility purpose or the `ARIA provider` will throw a warning that there is no `aria-label`.
- Usually, there is an `<md-icon>` element in the `<md-button>` attribute.
- Optionally, there also may be an `<md-tooltip>` element to provide tooltips for the button.

This example will be using [Material Icons by Google][1].

**index.html**:
<!-- language: lang-html -->
    <html ng-app="mdIconButtonApp">
        <head>
            <!-- Import Angular -->
            <script src="https://ajax.googleapis.com/ajax/libs/angularjs/1.5.11/angular.min.js"></script>
            <script src="https://ajax.googleapis.com/ajax/libs/angularjs/1.5.11/angular-animate.min.js"></script>
            <script src="https://ajax.googleapis.com/ajax/libs/angularjs/1.5.11/angular-aria.min.js"></script>
            <!-- Angular Material -->
            <script src="https://ajax.googleapis.com/ajax/libs/angular_material/1.5.11/angular-material.min.js"></script>
            <link href="https://ajax.googleapis.com/ajax/libs/angular_material/1.1.4/angular-material.min.css" rel="stylesheet">
            <!-- Material Icons -->
            <link href="https://fonts.googleapis.com/icon?family=Material+Icons"
      rel="stylesheet">
            <!-- Roboto -->
            <link href="https://fonts.googleapis.com/css?family=Roboto" rel="stylesheet">
            <!-- app.js -->
            <script src="app.js"></script>
            <style>
                body {
                    font-family: Roboto, sans-serif;
                }
            </style>
        </head>
        <body>
            <md-content ng-controller="mdIconButtonController">
                <!--
                Normal `md-button`
                Note that it is recommended to add a `aria-label` to `md-icon-button` for accessibility purposes.
                -->
                <md-button class="md-icon-button" aria-label="{{ariaLabel}}">
                    <md-icon class="material-icons">menu</md-icon>
                </md-button>
                <md-button class="md-primary md-icon-button" aria-label="{{ariaLabel}}">
                    <md-icon class="material-icons">menu</md-icon>
                    <md-tooltip>This is a tooltip!</md-tooltip>
                </md-button>
                <md-button class="md-accent md-icon-button md-fab" ng-click="goToLink('https://github.com/android')" aria-label="Go To Android">
                    <md-icon class="material-icons">android</md-icon>
                </md-button>
                <md-button class="md-warn md-icon-button" ng-href="{{link}}" aria-label="{{ariaLabel}}">
                    <md-icon class="material-icons">{{icon}}</md-icon>
                </md-button>
                <md-button class="md-raised md-icon-button" ng-click="goToLink('http://example.com')">
                    <!-- Note that you must have $mdIconProvider for this -->
                    <md-icon md-svg-icon="link"></md-icon>
                </md-button>
                <md-button class="md-cornered md-primary md-hue-1 md-icon-button" aria-label="{{ariaLabel}}">
                    <!-- You can also use the source of SVG -->
                    <md-icon md-svg-src="/path/to/more.svg"></md-icon>
                </md-button>
                <md-button class="md-accent md-hue-2" aria-label="{{ariaLabel}}">
                    <md-icon class="material-icons">g-translate</md-icon>
                </md-button>
                <md-button class="md-warn md-hue-3 md-icon-button" ng-href="{{link}}" aria-label="Link">
                    <md-icon md-svg-icon="copyright"></md-icon>
                </md-button>
            </md-content>
        </body>
    </html>

**app.js**:
<!-- language: lang-js -->
    angular.module('mdIconButtonApp', ['ngAnimate', 'ngAria', 'ngMaterial', 'ngMessages'])
    .config(function($mdIconProvider) {
        // Configure iconsets: More info: https://material.angularjs.org/latest/api/service/$mdIconProvider
        $mdIconProvider.iconSet('/path/to/media-icons.svg')
                       .defaultIconSet('/path/to/icons.svg')
                       .icon('sample-icon', '/path/to/sample-icon.svg');
    })
    .controller('mdIconButtonController', function($scope) {
    $scope.ariaLabel = "Button";
    $scope.icon = "menu";
    $scope.link = "https://google.com";
    $scope.goToLink = function(link) {
        // Go to some site
        console.log('Redirecting to:' + link);
        window.location.href=link;
        }
    })


  [1]: https://material.io/icons/
  [2]: https://fonts.google.com/specimen/Roboto

## Standard button
So, well, how on earth do you create a `<md-button>`, you may ask?
All you have to do is to enter a `<md-button>`, along with your text for the button in it.

**index.html**:
<!-- language: lang-html -->
    <div ng-app="MdButtonApp">
        <md-content ng-controller="Controller">
            <h2 class="md-title">Simple <code>md-button</code></h2>
            <md-button>Some button</md-button>
            </md-content>
    </div>

**app.js**:
<!-- language: lang-js -->
    // Just initialize the app
    angular.module('MdButtonApp', ['ngMaterial'])
        .controller('Controller', function($scope) {})

**[Codepen Demo][1]**


  [1]: http://codepen.io/Chan4077/pen/evJJeR "Codepen Demo"

