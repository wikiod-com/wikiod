---
title: "using native widget"
slug: "using-native-widget"
draft: false
images: []
weight: 9981
type: docs
toc: true
---

## Using surfaceView in ng2-TNS-Android : step by step
For example you want to use surfaceView in ng2-nativescript.
As we don't have `surfaceView` in nativescript we should use `placeholder`.

first we should import the requirements:

    import {Component} from "@angular/core";
    import placeholder = require("ui/placeholder");
    let application= require("application");


then add the placeholder to your html file:

`<Placeholder (creatingView)="creatingView($event)"></Placeholder>
`

Add this method to your class:

    public creatingView(args: any) {
      var nativeView = new android.view.SurfaceView(application.android.currentContext);
      args.view = nativeView;
    }
typescript doesn't know what is `android` and we should add platform declaration files
follow this [Answer][1] to add them.

because of a [problem][2]  in current version of ng2-nativescript we should do some extra work:

change the placeholder to :

    <Placeholder  *ngIf="init" (creatingView)="creatingView($event)"></Placeholder>
Import OnInit:

    import {Component,OnInit} from "@angular/core";

your class should implement OnInit

    export class AppComponent implements OnInit

 and add these lines to your class:

    public init: boolean = false;
    ngOnInit() {
        this.init = true;
    }
now you have a surfaceView in your nativescript app :)

**Call methods of SurfaceView**

For example you want to call `getHolder()`:


add a variable and loaded event to your placeholder like this:
    
      <Placeholder  #surface *ngIf="init" (creatingView)="creatingView($event)" (loaded)="onLoaded(surface)"></Placeholder>

and add the onLoaded method to your class:

     onLoaded(element){
      let mSurface = element.android;
      let holder =  mSurface.getHolder();
    }

**ATTENTION**:

It's not guaranteed that `android` property (`element.android`) will be available in `ngAfterViewInit` so we used `loaded` event instead of that.
    

  [1]: http://stackoverflow.com/a/37524018/4146943
  [2]: https://github.com/NativeScript/nativescript-angular/issues/283

## Using surfaceView in ng2-TNS-Android : whole ready example
**app.component.ts:**

    import {Component,OnInit} from "@angular/core";
    import placeholder = require("ui/placeholder");
    let application= require("application");

    @Component({
        selector: "my-app",
        templateUrl: "app.component.html",
    })
    export class AppComponent implements OnInit{

      public creatingView(args: any) {
        var nativeView = new android.view.SurfaceView(application.android.currentContext);
        args.view = nativeView;
      }

      onLoaded(element){
        let mSurface = element.android;
        let holder =  mSurface.getHolder();
      }

      public init: boolean = false;
        ngOnInit() {
            this.init = true;
        }
    }


**app.component.html :**

    <StackLayout>
       <Placeholder  #surface *ngIf="init" (creatingView)="creatingView($event)" (loaded)="onLoaded(surface)"></Placeholder>
    </StackLayout>



