---
title: "Accessing native apis"
slug: "accessing-native-apis"
draft: false
images: []
weight: 9979
type: docs
toc: true
---

## Write java code in nativescript and use it directly in javascript
This is the image of project structure in Android studio:

[![enter image description here][1]][1]

This is the image of project structure of nativescript project:

[![enter image description here][2]][2]


As you see they are same. so we can write java code in nativescript as we write in android studio.
 
We want to Add Toast to the default app of nativescript.
after creating a new nativescript project create a directory the  `java/org/example` directory like this:

[![enter image description here][3]][3]

create a new `MyToast.java` file in `example` directory;

***MyToast.java:***

    package org.example;

    import android.widget.Toast;
    import android.content.Context;


    public class MyToast{

        public static void showToast(Context context,String text ,String StrDuration ){
          int duration;
          switch (StrDuration){
              case "short":
                  duration = Toast.LENGTH_SHORT;
                  break;
              case "long":
                  duration = Toast.LENGTH_LONG;
                  break;
          }
            Toast.makeText(context,text, Toast.LENGTH_SHORT).show();
        }
    }

**Notes**: don't forget the package name;

***app.component.ts:***

    import {Component} from "@angular/core";
    let application = require("application");
    
    declare var org:any;
    @Component({
        selector: "my-app",
        templateUrl: "app.component.html",
    })
    export class AppComponent {
        public counter: number = 16;

        public get message(): string {
            if (this.counter > 0) {
                return this.counter + " taps left";
            } else {
                return "Hoorraaay! \nYou are ready to start building!";
            }
        }

        public onTap() {
            this.counter--;
            org.example.MyToast.showToast(application.android.context,"You pressed the button","short");
        }
    }

now when you press the button it will show a toast;

**Notes**:

1) showToast function accepts context to pass it to `Toast.makeText` an we passed a context to it in this way :`application.android.context`
2) typescript doesn't know what `org`is, so we declared it: `declare var org:any;`




  [1]: http://i.stack.imgur.com/ZuZau.png
  [2]: http://i.stack.imgur.com/A0x0W.png
  [3]: http://i.stack.imgur.com/TOShN.png

## use native apis directly in javascript
We want to add Toast to nativescript default app.

    import {Component} from "@angular/core";
    let application = require("application");

    declare var android:any;

    @Component({
        selector: "my-app",
        templateUrl: "app.component.html",
    })
    export class AppComponent {
        public counter: number = 16;

        public get message(): string {
            if (this.counter > 0) {
                return this.counter + " taps left";
            } else {
                return "Hoorraaay! \nYou are ready to start building!";
            }
        }

        public onTap() {
            this.counter--;
            this.showToast("You pressed the button","short");
        }



        public showToast(text:string ,StrDuration:string ):void{
          let duration:number;
          switch (StrDuration){
              case "short":
                  duration = android.widget.Toast.LENGTH_SHORT;
                  break;
              case "long":
                  duration = android.widget.Toast.LENGTH_LONG;
                  break;
          }    
            android.widget.Toast.makeText(application.android.context,text, android.widget.Toast.LENGTH_SHORT).show();
       }
    }

for creating toast we should call `Toast.makeText` and it's in the  `android.widget.Toast` package.
`Toast.makeText` accepts context as first argument and we can get the context in nativescript in this way:`application.android.context`


