---
title: "Use of in-built directives"
slug: "use-of-in-built-directives"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

## Hide/Show HTML Elements
This example hide show html elements.

    <!DOCTYPE html>
    <html ng-app="myDemoApp">
      <head>
        <script src="https://code.angularjs.org/1.5.8/angular.min.js"></script>
        <script>
    
          function HideShowController() {
            var vm = this;
            vm.show=false;
            vm.toggle= function() {
              vm.show=!vm.show;
            }
          }
          
          angular.module("myDemoApp", [/* module dependencies go here */])
            .controller("hideShowController", [HideShowController]);
        </script>
      </head>
      <body ng-cloak>
        <div ng-controller="hideShowController as vm">
          <a style="cursor: pointer;" ng-show="vm.show" ng-click="vm.toggle()">Show Me!</a>
          <a style="cursor: pointer;" ng-hide="vm.show" ng-click="vm.toggle()">Hide Me!</a>
        </div>
      </body>
    </html>

[Live Demo][1]

Step by step explanation:

 1. `ng-app="myDemoApp"`, the ngApp [directive][2] tells angular that a DOM element is controlled by a specific [angular.module][3] named "myDemoApp".
 2. `<script src="[//angular include]">` include angular js.
 3. `HideShowController` function is defined containing another function named `toggle` which help to hide show the element.
 4. `angular.module(...)` creates a new module.
 5. `.controller(...)` [Angular Controller][4] and returns the module for chaining;
 6. `ng-controller` [directive][4] is key aspect of how angular supports the principles behind the Model-View-Controller design pattern.
 7. `ng-show` [directive][5] shows the given HTML element if expression provided is true.
 8. `ng-hide` [directive][6] hides the given HTML element if expression provided is true.
 9. `ng-click` [directive][7] fires a toggle function inside controller  


  [1]: https://plnkr.co/edit/wbSmMu96Xz3svv0qilwh?p=preview
  [2]: https://docs.angularjs.org/api/ng/directive/ngApp
  [3]: https://docs.angularjs.org/api/ng/function/angular.module
  [4]: https://docs.angularjs.org/api/ng/directive/ngController
  [5]: https://docs.angularjs.org/api/ng/directive/ngShow
  [6]: https://docs.angularjs.org/api/ng/directive/ngHide
  [7]: https://docs.angularjs.org/api/ng/directive/ngClick

