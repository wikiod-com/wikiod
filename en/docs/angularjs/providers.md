---
title: "Providers"
slug: "providers"
draft: false
images: []
weight: 9915
type: docs
toc: true
---

## Syntax
 * constant(name, value);
 * value(name, value); 
 * factory(name, $getFn);
 * service(name, constructor);
 * provider(name, provider);

Providers are singleton objects that can be injected, for example, into other services, controllers and directives. All providers are registered using different "recipes", where `Provider` is the most flexible one. All possible recipes are:

 * Constant
 * Value
 * Factory
 * Service
 * Provider

Services, Factories and Providers are all lazy initialized, component is initialized only if application depends on it.

[Decorators][1] are closely related to Providers. Decorators are used to intercept service or factory creation in order to change it's behavior or override (parts of) it.


  [1]: https://www.wikiod.com/angularjs/decorators

## Provider
`Provider` is available both in configuration and run phases.

> The Provider recipe is syntactically defined as a custom type that
> implements a `$get` method.
> 
> You should use the Provider recipe only
> when you want to expose an API for application-wide configuration that
> must be made before the application starts. This is usually
> interesting only for reusable services whose behavior might need to
> vary slightly between applications.

    angular.module('app',[])
      .provider('endpointProvider', function() {
        var uri = 'n/a';
        
        this.set = function(value) {
          uri = value;
        };
    
        this.$get = function() {
          return {
            get: function() {
              return uri;
            }
          };
        };
      })
      .config(function(endpointProviderProvider) {
        endpointProviderProvider.set('http://some.rest.endpoint');
      })   
      .controller('MainCtrl', function(endpointProvider) {
        var vm = this;
        vm.endpoint = endpointProvider.get();
      }); 

<hr>

    <body ng-controller="MainCtrl as vm">
      <div>endpoint = {{::vm.endpoint }}</div>
    </body>

<hr>

> endpoint = http://some.rest.endpoint

Without `config` phase result would be

> endpoint = n/a

## Factory
`Factory` is available in run phase.

> The Factory recipe constructs a new service using a function with zero
> or more arguments (these are dependencies on other services). The
> return value of this function is the service instance created by this
> recipe.
> 
> Factory can create a service of any type, whether it be a primitive, object 
> literal, function, or even an instance of a custom type.

    angular.module('app',[])
      .factory('endpointFactory', function() {
        return {
          get: function() {
            return 'http://some.rest.endpoint';
          }
        };
      })
      .controller('MainCtrl', function(endpointFactory) {
        var vm = this;
        vm.endpoint = endpointFactory.get();
      });

<hr>

    <body ng-controller="MainCtrl as vm">
      <div>endpoint = {{::vm.endpoint }}</div>
    </body>

<hr>

> endpoint = http://some.rest.endpoint

## Constant
`Constant` is available both in configuration and run phases.

    angular.module('app',[])
      .constant('endpoint', 'http://some.rest.endpoint') // define
      .config(function(endpoint) {
        // do something with endpoint
        // available in both config- and run phases
      }) 
      .controller('MainCtrl', function(endpoint) {       // inject
        var vm = this;
        vm.endpoint = endpoint;                          // usage
      });

<hr>

    <body ng-controller="MainCtrl as vm">
      <div>endpoint = {{ ::vm.endpoint }}</div>
    </body>

<hr>

> endpoint = http://some.rest.endpoint



## Service
`Service` is available in run phase.

> The Service recipe produces a service just like the Value or Factory
> recipes, but it does so by *invoking a constructor with the new
> operator*. The constructor can take zero or more arguments, which
> represent dependencies needed by the instance of this type.

    angular.module('app',[])
      .service('endpointService', function() {
        this.get = function() {
          return 'http://some.rest.endpoint';
        };
      })
      .controller('MainCtrl', function(endpointService) {
        var vm = this;
        vm.endpoint = endpointService.get();
      });

<hr>

    <body ng-controller="MainCtrl as vm">
      <div>endpoint = {{::vm.endpoint }}</div>
    </body>

<hr>

> endpoint = http://some.rest.endpoint

## Value
`Value` is available both in configuration and run phases.

    angular.module('app',[])
      .value('endpoint', 'http://some.rest.endpoint') // define
      .run(function(endpoint) {
        // do something with endpoint
        // only available in run phase
      }) 
      .controller('MainCtrl', function(endpoint) {    // inject
        var vm = this;
        vm.endpoint = endpoint;                       // usage
      }); 

<hr>

    <body ng-controller="MainCtrl as vm">
      <div>endpoint = {{ ::vm.endpoint }}</div>
    </body>

<hr>

> endpoint = http://some.rest.endpoint



