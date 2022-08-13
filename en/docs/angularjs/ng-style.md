---
title: "ng-style"
slug: "ng-style"
draft: false
images: []
weight: 9986
type: docs
toc: true
---

The 'ngStyle' directive allows you to set CSS style on an HTML element conditionally.
Much like how we could use *style* attribute on HTML element in non-AngularJS projects, we can use `ng-style` in angularjs do apply styles based on some boolean condition. 

## Syntax
 

 - `<ANY ng-style="expression"></ANY >`  

 -  `<ANY class="ng-style:
       expression;"> ... </ANY>`

## Use of ng-style
   
Below example changes the opacity of the image based on the "status" parameter.

     <img class="img-responsive"  ng-src="{{imagesrc}}"
         ng-style="{'opacity' : (status == 2) ? 1 : 0.5}">

