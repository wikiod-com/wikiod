---
title: "Feature Modules"
slug: "feature-modules"
draft: false
images: []
weight: 9840
type: docs
toc: true
---

## A Feature Module
<!-- language: lang-typescript -->

    // my-feature.module.ts
    import { CommonModule } from '@angular/common';
    import { NgModule }     from '@angular/core';

    import { MyComponent } from './my.component';
    import { MyDirective } from './my.directive';
    import { MyPipe }      from './my.pipe';
    import { MyService }   from './my.service';

    @NgModule({
      imports:      [ CommonModule ],
      declarations: [ MyComponent, MyDirective, MyPipe ],
      exports:      [ MyComponent ],
      providers:    [ MyService ]
    })
    export class MyFeatureModule { }

Now, in your root (usually `app.module.ts`):

<!-- language: lang-typescript -->

    // app.module.ts
    import { NgModule }      from '@angular/core';
    import { BrowserModule } from '@angular/platform-browser';
    
    import { AppComponent }    from './app.component';
    import { MyFeatureModule } from './my-feature.module';
    
    @NgModule({
      // import MyFeatureModule in root module
      imports:      [ BrowserModule, MyFeatureModule ],
      declarations: [ AppComponent ],
      bootstrap:    [ AppComponent ]
    })
    export class AppModule { }

