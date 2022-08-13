---
title: "Bootstrap Empty module in angular 2"
slug: "bootstrap-empty-module-in-angular-2"
draft: false
images: []
weight: 9704
type: docs
toc: true
---

## An empty module
```
import { NgModule } from '@angular/core';

@NgModule({
  declarations: [], // components your module owns.
  imports: [], // other modules your module needs.
  providers: [], // providers available to your module.
  bootstrap: [] // bootstrap this root component.
})
export class MyModule {}
```

This is an empty module containing no declarations, imports, providers, or components to bootstrap. Use this a reference.

## Application Root Module
    import { NgModule }      from '@angular/core';
    import { BrowserModule } from '@angular/platform-browser';
    
    import { AppComponent }  from './app.component';
    
    @NgModule({
      imports: [ BrowserModule ],
      declarations: [ AppComponent ],
      bootstrap:    [ AppComponent ]
    })
    export class AppModule { }



## Bootstrapping your module
```
import { platformBrowserDynamic } from '@angular/platform-browser-dynamic';
import { MyModule }               from './app.module';

platformBrowserDynamic().bootstrapModule( MyModule );
```

In this example, `MyModule` is the module containing your root component. By bootstrapping `MyModule` your Angular 2 app is ready to go.

## A module with networking on the web browser.
```
// app.module.ts

import { NgModule } from '@angular/core';
import { BrowserModule } from '@angular/platform-browser';
import { HttpModule } from '@angular/http';
import { MyRootComponent } from './app.component';

@NgModule({
  declarations: [MyRootComponent],
  imports: [BrowserModule, HttpModule],
  bootstrap: [MyRootComponent]
})
export class MyModule {}
```

`MyRootComponent` is the root component packaged in `MyModule`. It is the entry point to your Angular 2 application.

## Static bootstrapping with factory classes
We can statically bootstrap an application by taking the plain ES5 Javascript output of the generated factory classes. Then we can use that output to bootstrap the application:

    import { platformBrowser } from '@angular/platform-browser';
    import { AppModuleNgFactory } from './main.ngfactory';
    
    // Launch with the app module factory.
    platformBrowser().bootstrapModuleFactory(AppModuleNgFactory);

This will cause the application bundle to be much smaller, because all the template compilation was already done in a build step, using either ngc or calling its internals directly.

