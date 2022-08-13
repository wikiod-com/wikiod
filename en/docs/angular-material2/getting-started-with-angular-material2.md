---
title: "Getting started with angular-material2"
slug: "getting-started-with-angular-material2"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Wrapping all modules together
You can also easily wrap all angular modules, which you are going to use, into one module:
    
<!-- language: lang-ts -->

    import { NgModule } from '@angular/core';
    import { MdButtonModule, MdSnackBarModule, MdSidenavModule } from '@angular/material';
     
    @NgModule({
        imports: [
            BrowserAnimationsModule,
            MdButtonModule,
            MdSnackBarModule,
            MdSidenavModule
        ],
        exports: [
            MdButtonModule,
            MdSnackBarModule,
            MdSidenavModule
        ]
    })
    export class MaterialWrapperModule {}


After that simply import your module into the application main module:

<!-- language: lang-ts -->

    import { NgModule } from '@angular/core';
    import { CommonModule } from '@angular/common';
    import { RouterModule } from '@angular/router';
    
    import { MaterialWrapperModule } from './material-wrapper.module.ts';
    import { AppComponent } from './app.component';
    
    
    @NgModule({
        imports: [
            BrowserAnimationsModule, 
            MaterialWrapperModule,
            CommonModule,
            // This is optional, use it when you would like routing in your app
            // RouterModule.forRoot([
            //     { path: '', component: HomeView, pathMatch: 'full'}
            // ])
        ],
        declarations: [ AppComponent],
        bootstrap: [ AppComponent ]
    })
    export class AppModule {}

## Installation and Setup from master with Angular CLI
This example will be how to install from master and will be using `@angular/cli` as well.

1. Make a new project with `@angular/cli`:

   <!-- language: lang-bash -->
       ng new my-master-app

   _If you haven't installed `@angular/cli`, use this command:_

   <!-- language: lang-bash -->
       npm install -g @angular/cli
2. Install from `master`:
   <!-- language: lang-bash -->
       npm install --save @angular/animations
       npm install --save angular/material2-builds
       npm install --save angular/cdk-builds
3. Follow the same guide as above.

Done!

## Installation or Setup with Angular CLI
In this example, we will be using `@angular/cli` (latest) and the latest version of `@angular/material`. You should at least know the basics of Angular 2/4 before continuing the steps below.

1. Install angular material module from `npm`:

   <!-- language: lang-bash -->
       npm install @angular/material --save

<!-- if version [gte 2.0.0-beta.3] -->
_This only applies to versions `2.0.0-beta.3` and up._

Install the `@angular/animations` module:

<!-- language: lang-bash -->
    
    npm install @angular/animations --save
<!-- end version if -->
<!-- if version [gte 2.0.0-beta.8] -->
_This only applies to versions `2.0.0-beta.8` and up._

Install the `@angular/cdk` module:

<!-- language: lang-bash -->

    npm install @angular/cdk --save

<!-- end version if -->
2. In your application module import the components which you are going to use:
    
    <!-- language: lang-ts -->
       import { NgModule } from '@angular/core';
       import { CommonModule } from '@angular/common';
       import { RouterModule } from '@angular/router';
       import { MdButtonModule, MdSnackBarModule, MdSidenavModule } from '@angular/material';
    
       import { AppComponent } from './app.component';
    
       import { BrowserAnimationsModule } from '@angular/platform-browser/animations';
    
       @NgModule({
            imports: [
                BrowserAnimationsModule,
                MdButtonModule,
                MdSnackBarModule,
                MdSidenavModule,
                CommonModule,
                // This is optional unless you want to have routing in your app
                // RouterModule.forRoot([
                //     { path: '', component: HomeView, pathMatch: 'full'}
                // ])
            ],
            declarations: [ AppComponent ],
            boostrap: [ AppComponent ]
       })
       export class AppModule {}


You are now ready to use Angular Material in your components!

**Note: The docs for specific components will be coming soon.**

## Set up theme, gesture support and material icons
----------
**Theme:**

*A theme is **required** for material components to work properly within the application.* 

Angular Material 2 provides four prebuilt themes:

- deeppurple-amber
- indigo-pink
- pink-bluegrey
- purple-green

If you are using **Angular CLI**, you can import one of the prebuilt themes in `style.css`.

    @import "~@angular/material/prebuilt-themes/indigo-pink.css";

Theme can be added using `<link>` in `index.html` as well.

    <link href="node_modules/@angular/material/prebuilt-themes/indigo-pink.css" rel="stylesheet">



----------
**HammerJS**

Add HammerJS to the application via [CDN][2] or `npm`:

    npm install --save hammerjs

In your root module, usually `app.module.ts`, add the import statement:

    import 'hammerjs';

----------
**Material Icons:**

Unless, custom icons provided, Angular Material 2 `<md-icon>` expects Material Icons.

In `index.html` add:

    <link href="https://fonts.googleapis.com/icon?family=Material+Icons" rel="stylesheet">


  [1]: https://material.angular.io/guide/theming
  [2]: https://developers.google.com/speed/libraries/#hammerjs

