---
title: "Use native webcomponents in Angular 2"
slug: "use-native-webcomponents-in-angular-2"
draft: false
images: []
weight: 9978
type: docs
toc: true
---

When you use a web component in your Angular 2 template, angular will try to find a component with a selector matching the custom tag of the web component - which it of course can't and will throw an error. 

The solution is to import a "custom elements schema" in the module that holds the component. This will make angular accept any custom tag, that doesn't match any ng component selector.

## Include custom elements schema in your module
    import { NgModule, CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
    import { CommonModule } from '@angular/common';
    import { AboutComponent } from './about.component';
    
    @NgModule({
        imports: [ CommonModule ],
        declarations: [ AboutComponent ],
        exports: [ AboutComponent ],
        schemas: [ CUSTOM_ELEMENTS_SCHEMA ]
    })
    
    export class AboutModule { }

## Use your webcomponent in a template
    import { Component } from '@angular/core';

    @Component({
      selector: 'myapp-about',
      template: `<my-webcomponent></my-webcomponent>`
    })
    export class AboutComponent { }

