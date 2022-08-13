---
title: "Lazy loading"
slug: "lazy-loading"
draft: false
images: []
weight: 9953
type: docs
toc: true
---

 1. If your lazy loaded dependencies require other lazy loaded dependencies make sure you load them in the right order!


    angular.module('lazy', [
     'alreadyLoadedDependency1',
     'alreadyLoadedDependency2',
     ...
     {
      files: [
      'path/to/lazily/loaded/dependency1.js',
      'path/to/lazily/loaded/dependency2.js', //<--- requires lazily loaded dependency1
      'path/to/lazily/loaded/dependency.css'
      ],
      serie: true //Sequential load instead of parallel
     }
    ]);

## Preparing your project for lazy loading
After including `oclazyload.js` in your index file, declare `ocLazyLoad` as a dependency in `app.js`

    //Make sure you put the correct dependency! it is spelled different than the service!
    angular.module('app', [
     'oc.lazyLoad',
     'ui-router'
    ])

## Usage
In order to lazily load files inject the `$ocLazyLoad` service into a controller or another service

    .controller('someCtrl', function($ocLazyLoad) {
     $ocLazyLoad.load('path/to/file.js').then(...);
    });

Angular modules will be automatically loaded into angular.

Other variation:

    $ocLazyLoad.load([
      'bower_components/bootstrap/dist/js/bootstrap.js',
      'bower_components/bootstrap/dist/css/bootstrap.css',
      'partials/template1.html'
    ]);

For a complete list of variations visit the [official][1] documentation


  [1]: https://oclazyload.readme.io/docs/oclazyload-service

## Usage with router
## UI-Router: ##

    .state('profile', {
     url: '/profile',
     controller: 'profileCtrl as vm'
     resolve: {
      module: function($ocLazyLoad) {
       return $ocLazyLoad.load([
        'path/to/profile/module.js',
        'path/to/profile/style.css'
       ]);
      }
     }
    });

## ngRoute: ##

    .when('/profile', {
         controller: 'profileCtrl as vm'
         resolve: {
          module: function($ocLazyLoad) {
           return $ocLazyLoad.load([
            'path/to/profile/module.js',
            'path/to/profile/style.css'
           ]);
          }
         }
        });

## Using dependency injection
The following syntax allows you to specify dependencies in your `module.js` instead of explicit specification when using the service

    //lazy_module.js
    angular.module('lazy', [
     'alreadyLoadedDependency1',
     'alreadyLoadedDependency2',
     ...
     [
      'path/to/lazily/loaded/dependency.js',
      'path/to/lazily/loaded/dependency.css'
     ]
    ]);

**Note**: this syntax will only work for lazily loaded modules!

## Using the directive
    <div oc-lazy-load="['path/to/lazy/loaded/directive.js', 'path/to/lazy/loaded/directive.html']">
    
    <!-- myDirective available here -->
    <my-directive></my-directive>
    
    </div>

