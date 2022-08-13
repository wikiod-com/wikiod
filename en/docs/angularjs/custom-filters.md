---
title: "Custom filters"
slug: "custom-filters"
draft: false
images: []
weight: 9961
type: docs
toc: true
---

## Simple filter example
Filters format the value of an expression for display to the user. They can be used in view templates, controllers or services. This example creates a filter (`addZ`) then uses it in a view.  All this filter does is add a capital 'Z' to the end of the string.

## example.js
       
    angular.module('main', [])
        .filter('addZ', function() {
                return function(value) {
                    return value + "Z";
                }
         })
        .controller('MyController', ['$scope', function($scope) {
            $scope.sample = "hello";
        }])

## example.html

Inside the view, the filter is applied with the following syntax: `{ variable | filter}`.  In this case, the variable we defined in the controller, `sample`, is being filtered by the filter we created, `addZ`.

    <div ng-controller="MyController">
       <span>{{sample | addZ}}</span>
    </div>

## Expected output

    helloZ

## Use a filter in a controller, a service or a filter
You will have to inject `$filter`:
 
```
angular
  .module('filters', [])
  .filter('percentage', function($filter) {
    return function (input) {
      return $filter('number')(input * 100) + ' %';
    };
  });
```


## Create a filter with parameters
By default, a filter has a single parameter: the variable it is applied on. But you can pass more parameter to the function:

```
angular
  .module('app', [])
  .controller('MyController', function($scope) {
    $scope.example = 0.098152;
  })
  .filter('percentage', function($filter) {
    return function (input, decimals) {
      return $filter('number')(input * 100, decimals) + ' %';
    };
  });
```

Now, you can give a precision to the `percentage` filter:

```
<span ng-controller="MyController">{{ example | percentage: 2 }}</span>
=> "9.81 %"
```

... but other parameters are optional, you can still use the default filter:

```
<span ng-controller="MyController">{{ example | percentage }}</span>
=> "9.8152 %"
```


