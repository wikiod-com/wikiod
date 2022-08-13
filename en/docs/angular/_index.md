---
title : Angular Tutorial
slug : angular-tutorial
weight : 9923
draft : false
images : []
type : docs
---

**Angular** (commonly referred to as "**Angular 2+**" or "**Angular 2**") is a [TypeScript][1]-based open-source front-end web framework led by the [Angular Team][2] at Google and by a community of individuals and corporations to address all of the parts of the developer's workflow while building complex web applications. Angular is a complete rewrite from the same team that built [AngularJS][3]. [¹][4]

The framework consists of [several libraries][5], some of them core ([@angular/core][6] for example) and some optional ([@angular/animations][7]).

You write Angular applications by composing [HTML _templates_][8] with Angularized markup, writing [_component_][9] classes to manage those templates, adding application logic in [_services_][10], and boxing components and services in [_modules_][11].

Then you launch the app by [_bootstrapping_][12] the _root module_. Angular takes over, presenting your application content in a browser and responding to user interactions according to the instructions you've provided.

Arguably, the most fundamental part of developing Angular applications are the **components**. A component is the combination of an HTML template and a component class that controls a portion of the screen. Here is an example of a component that displays a simple string:

> *src/app/app.component.ts*
> 
>     import { Component } from '@angular/core';
>     
>     @Component({
>         selector: 'my-app',
>         template: `<h1>Hello {{name}}</h1>`
>     })
>     export class AppComponent {
>         name = 'Angular';
>     }

Every component begins with a `@Component` decorator function that takes a [metadata][13] object. The metadata object describes how the HTML template and component class work together.

The `selector` property tells Angular to display the component inside a custom `<my-app>` tag in the *index.html* file.

> *index.html (inside the `body` tag)*
> 
>     <my-app>Loading AppComponent content here ...</my-app>

The template property defines a message inside a `<h1>` header. The message starts with "Hello" and ends with `{{name}}`, which is an Angular [interpolation binding][14] expression. At runtime, Angular replaces `{{name}}` with the value of the component's `name` property. Interpolation binding is one of many Angular features you'll discover in this documentation. In the example, change the component class's `name` property from `'Angular'` to `'World'` and see what happens.

This example is written in **TypeScript**, a superset of JavaScript. Angular uses TypeScript because its types make it easy to support developer productivity with tooling. Additionally, **almost all support is for TypeScript** and so using **plain JavaScript** to write your application will be **difficult**. Writing Angular code in JavaScript is possible, however; [this guide][15] explains how.

More information on the **architecture** of Angular can be found **[here][16]**


  [1]: http://www.typescriptlang.org/
  [2]: https://angular.io/about/
  [3]: https://angularjs.org/
  [4]: https://en.wikipedia.org/wiki/Angular_(application_platform)
  [5]: https://www.npmjs.com/~angular
  [6]: https://www.npmjs.com/package/@angular/core
  [7]: https://www.npmjs.com/package/@angular/animations
  [8]: https://angular.io/docs/ts/latest/guide/template-syntax.html
  [9]: https://angular.io/docs/ts/latest/api/core/index/Component-decorator.html
  [10]: https://angular.io/docs/ts/latest/glossary.html#!#service
  [11]: https://angular.io/docs/ts/latest/glossary.html#!#module
  [12]: https://angular.io/docs/ts/latest/glossary.html#!#bootstrap
  [13]: https://angular.io/docs/ts/latest/guide/architecture.html#!#metadata
  [14]: https://angular.io/docs/ts/latest/guide/displaying-data.html
  [15]: https://angular.io/docs/ts/latest/cookbook/ts-to-js.html
  [16]: https://angular.io/docs/ts/latest/guide/architecture.html

