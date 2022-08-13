---
title: "Using Tabs"
slug: "using-tabs"
draft: false
images: []
weight: 9903
type: docs
toc: true
---

Always remember to check out [Ionic 2 Tab docs](http://ionicframework.com/docs/v2/api/components/tabs/Tabs/) to be aware of the latest changes and updates.

## Change selected tab programatically from child Page
You can take a look at the full code in this [working Plunker](http://plnkr.co/edit/4ZmJyO?p=preview).

In this example I use a shared service to handle the communication between the pages inside the tab (child pages) and the tab container (the component that holds the tabs). Even though you probably could do it with [Events](http://ionicframework.com/docs/v2/api/util/Events/) I like the shared service approach because is easier to understand and also to mantain when the application starts growing.

**TabService**

    import {Injectable} from '@angular/core';
    import {Platform} from 'ionic-angular/index';
    import {Observable} from 'rxjs/Observable';
     
    @Injectable()
    export class TabService { 
      
      private tabChangeObserver: any;
      public tabChange: any;
    
      constructor(private platform: Platform){
        this.tabChangeObserver = null;
        this.tabChange = Observable.create(observer => {
            this.tabChangeObserver = observer;
        });
      }
      
      public changeTabInContainerPage(index: number) {
        this.tabChangeObserver.next(index);
      }
    }

So basically the `TabService` only creates an `Observable` to allow the tabs container to subscribe to it, and also declares the `changeTabInContainerPage()` method that will be called from the child pages.

Then, in each child page (the ones inside the tabs) we only add a button and bind the `click` event to a method that calls the service:

**Page1.html**

    <ion-content class="has-header">
        <h1>Page 1</h1>
        <button secondary (click)="changeTab()">Select next tab</button>
    </ion-content>

**Page1.ts**

    import { Component } from '@angular/core';
    import { Observable } from 'rxjs/Observable';
    import { TabService } from 'tabService.ts';
    
    @Component({
      templateUrl:"page1.html"
    })
    export class Page1 {
    
      constructor(private tabService: TabService) { }
    
      public changeTab() {
        this.tabService.changeTabInContainerPage(1);
      }
    }

And finally, in the `TabsPage`, we only subscribe to the service, and then we change the selected tab with `this.tabRef.select(index);`

    import { Component, ViewChild } from "@angular/core";
    import { Page1 } from './page1.ts';
    import { Page2 } from './page2.ts';
    import { TabService } from 'tabService.ts'; 
    
    @Component({
      templateUrl: 'tabs.html'
    })
    export class TabsPage {
      @ViewChild('myTabs') tabRef: Tabs;
    
      tab1Root: any = Page1;
      tab2Root: any = Page2;
    
      constructor(private tabService: TabService){
        this.tabService.tabChange.subscribe((index) => {
          this.tabRef.select(index);
        });
      }
    }

Please notice that we're getting a reference to the Tabs instance by adding `#myTabs` in the `ion-tabs` element, and we get it from the component with `@ViewChild('myTabs') tabRef: Tabs;`

    <ion-tabs #myTabs>
      <ion-tab [root]="tab1Root" tabTitle="Tab 1"></ion-tab>
      <ion-tab [root]="tab2Root" tabTitle="Tab 2"></ion-tab>
    </ion-tabs>

## Change tab with selectedIndex
Instead of getting a reference to the DOM you can simply change the index of the tab using the selectedIndex attribute on the ion-tabs

HTML:

     <ion-tabs [selectedIndex]="tabIndex" class="tabs-icon-text" primary >
          <ion-tab tabIcon="list-box"   [root]="tabOne"></ion-tab>
          <ion-tab tabIcon="contacts"  [root]="tabTwo"></ion-tab>
          <ion-tab tabIcon="chatboxes"  [tabBadge]="messagesReceived" [root]="tabFive"></ion-tab> 
     </ion-tabs>

TS:
   
     import { Events} from "ionic-angular";

     export class tabs {
      public tabIndex: number;
      constructor(e: Events) {
        tabs.mySelectedIndex = navParams.data.tabIndex || 0;
        e.subscribe("tab:change", (newIndex) => this.tabIndex = newIndex);
      }
    }



If you want to change it from some other controller service you can send an event:

    e.publish("tab:change",2);



