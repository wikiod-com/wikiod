---
title: "Custom parameter types"
slug: "custom-parameter-types"
draft: false
images: []
weight: 9963
type: docs
toc: true
---

## Parameters
| Parameter| Details|
| ------ | ------ |
| `decode` | Converts URL value (string) to the value available in `$stateParams`|
| `encode` | Converts a value to the string that will be used in the URL |
| `equals` | Verifies if two values are equal from the type's point of view |
| `is` | Checks if the value can be used as defined parameter type |
| `pattern` | Ensures that the values from URL matches this pattern when route resolves |

## Path parameter (with not-encoded slash inside)
By default, ui-router encodes the slash `/` inside parameters. If you want to send a path in the URL, you need to define a custom parameter type.

Define:
    
    module.config(['$urlMatcherFactoryProvider', function($urlMatcherFactory) {
      $urlMatcherFactory.type('path', {
        decode: function(val) { return val != null ? val.toString() : val; },
        encode: function(val) { return val != null ? val.toString() : val; },
        is: function(val) { return this.pattern.test(val); },
        pattern: /[^/]+\/[^/]+/
      })
    }]);

And use:

    $stateProvider.state({
      url: '/my-route/{directory:path}'
      template: '<my-page></my-page>'
    });

[Related question](http://stackoverflow.com/questions/27849260/angular-ui-sref-encode-parameter).


## Page number parameter
Similar to `int` but accepts only positive integers (useful for pagination when there is a `page` parameter.

Define:
    
    module.config(['$urlMatcherFactoryProvider', function($urlMatcherFactory) {
      $urlMatcherFactory.type('page', {
        decode: function(val) { return +val; },
        encode: function(val) { return Math.floor(val); },
        equals: function(a, b) { return this.is(a) && +a == +b; },
        is: function(val) { return angular.isNumber(val) && val >= 1; },
        pattern: /\d+/
      })
    }]);

And use:

    $stateProvider.state({
      url: '/my-route/{page:page}'
      template: '<my-page></my-page>'
    });

[Plunker](http://plnkr.co/edit/tU6yPke2CCGrclgVa2Rb) and [related SO answer](http://stackoverflow.com/a/27182177/878514).

## Boolean parameter
Define:
    
    module.config(['$urlMatcherFactoryProvider', function($urlMatcherFactory) {
      $urlMatcherFactory.type('boolean', {
        decode: function(val) { return val == true || val == "true" },
        encode: function(val) { return val ? 1 : 0; },
        equals: function(a, b) { return this.is(a) && a == b; },
        is: function(val) { return [true, false, 0, 1].indexOf(val) >= 0 },
        pattern: /false|true|0|1/
      })
    }]);

And use:

    $stateProvider.state({
      url: '/my-route/{showSidebar:boolean}'
      template: '<my-page></my-page>'
    });

[Plunker](http://plnkr.co/edit/tU6yPke2CCGrclgVa2Rb) and [related SO answer](http://stackoverflow.com/a/27182177/878514).

