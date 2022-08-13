---
title: "Getting started with angular-ui-bootstrap"
slug: "getting-started-with-angular-ui-bootstrap"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation or Setup
The  official site of angular-ui-bootstrap is [here][1].

Follow the below instructions in order.
The list of files that are to be downloaded is in this [link][2]

Include all references in this order. 

 1. angular.js
 2. angular-animate.js
 3. ui-bootstrap-tpls-2.2.0.js (Reference to UI Bootstrap ) 
 4. angular-sanitize.js

Note: 

 - It is important that all of the above scripts are referenced and done so in the order shown above.


Here are a few CDN LINKS to reference these files

    //ajax.googleapis.com/ajax/libs/angularjs/1.5.8/angular.js
    //ajax.googleapis.com/ajax/libs/angularjs/1.5.8/angular-animate.js
    //ajax.googleapis.com/ajax/libs/angularjs/1.5.8/angular-sanitize.js
    //angular-ui.github.io/bootstrap/ui-bootstrap-tpls-2.2.0.js

Inject the dependency for ui-bootstrap into your module as 

    angular.module('myApp', ['ui.bootstrap']);

Your module is setup to work with angular-ui-bootstrap.

**Common Errors:**
  1. **[$injector:modulerr]**
        
      Solution: 
 - Ensure that script files are included 
 - For CDN references, you must include the **//** before the link which
   uses either http or https to include those scripts.

2. **Component will not be displayed or behaviour is not as expected.**

     Solution:
 - Ensure that the mandatory settings for the corresponding directives
   are available in your Controller.

  [1]: https://angular-ui.github.io/bootstrap/
  [2]: https://github.com/angular-ui/bootstrap/tree/gh-pages

