---
title: "Displaying data as list (using Repeater, ListView or *ngFor for {N}+Angular-2 apps)"
slug: "displaying-data-as-list-using-repeater-listview-or-ngfor-for-n+angular-2-apps"
draft: false
images: []
weight: 9886
type: docs
toc: true
---

>> Note: Don't use Repeater in  {N}+Angular-2 applications! The *ngRepeat is obsolete directive in Angular-2. When you need to display repeating item patterns use either ListView or *ngFor structural directive.

## Using *ngFor Structural Directive to display data (nativeScript + Angular-2)
*ngfor.component.html*

    <StackLayout>
        <Label *ngFor="let item of items" [text]="item"></Label>
    </StackLayout>

*ngfor.component.ts*

    import { Component } from "@angular/core";

    var dataItems = ["data-item 1", "data-item 2", "data-item 3"]

    @Component({
        selector: 'ngfor-component',
        styleUrls:["./ngfor.component.css"],
        templateUrl: "./ngfor.component.html",
    })
    
    export class NgForComponent {
        public items:Array<string> = [];
    
        constructor(){
            this.items = dataItems;
        }
    }



## Using Repeater module to display data (NativeScript Core)
*page.xml*

    <Page xmlns="http://schemas.nativescript.org/tns.xsd" navigatingTo="navigatingTo">
      <Repeater items="{{ myItems }}">
          <Repeater.itemTemplate>
            <Label text="{{ title || 'Downloading...' }}" textWrap="true" />
          </Repeater.itemTemplate>
      </Repeater>
    </Page>

*page.ts*

    import { EventData, Observable } from "data/observable";
    import { Page } from "ui/page";
    
    let viewModel = new Observable();
    var myItems = [ {title: "Core Concepts"}, 
                    {title: "User Interface"}, 
                    {title: "Plugins"}, 
                    {title: "Cookbook"}, 
                    {title: "Tutorials"} ];
    
    export function navigatingTo(args: EventData) {
        var page = <Page>args.object;

        viewModel.set("myItems", myItems);
    
        page.bindingContext = viewModel;
    }

## Using Repeater module with ObservableArray (NativeScript Core)
*page.xml*

    <Page xmlns="http://schemas.nativescript.org/tns.xsd" navigatingTo="navigatingTo">
      <Repeater items="{{ myItems }}">
          <Repeater.itemTemplate>
            <Label text="{{ title || 'Downloading...' }}" textWrap="true" class="title" />
          </Repeater.itemTemplate>
      </Repeater>
    </Page>

*page.ts*

    import { EventData, Observable } from "data/observable";
    import { ObservableArray } from "data/observable-array";
    import { Page } from "ui/page";
    
    let viewModel = new Observable();
    let myItems = new ObservableArray({title: "Core Concepts"}, {title: "User Interface"}, {title: "Plugins"}, {title: "Cookbook"}, {title: "Tutorials"});
    
    export function navigatingTo(args: EventData) {
    
        var page = <Page>args.object;
        viewModel.set("myItems", myItems);
    
        // The Repeater will be updated automatically when new item is pushed.
        myItems.push({title:"Publishing"});
    
        page.bindingContext = viewModel;
    }

## Using ListView module with ObservableArray (NativeScript Core)
*page.xml*

    <Page xmlns="http://schemas.nativescript.org/tns.xsd" navigatingTo="navigatingTo">
      <ListView items="{{ myItems }}" itemTap="listViewItemTap">
          <ListView.itemTemplate>
            <Label text="{{ title || 'Downloading...' }}" textWrap="true" class="title" />
          </ListView.itemTemplate>
      </ListView>
    </Page>

*page.ts*

    import { EventData, Observable } from "data/observable";
    import { ObservableArray } from "data/observable-array";
    import { Page } from "ui/page";
    import { ItemEventData } from "ui/list-view";
    
    import frameModule = require("ui/frame");
    
    let viewModel = new Observable();
    let myItems = new ObservableArray(  {title: "Core Concepts"}, 
                                        {title: "User Interface"}, 
                                        {title: "Plugins"}, 
                                        {title: "Cookbook"}, 
                                        {title: "Tutorials"}  );
    
    export function navigatingTo(args: EventData) {
    
        var page = <Page>args.object;
        viewModel.set("myItems", myItems);
    
        // ListView will be updated automatically when new item is pushed.
        myItems.push({title:"Publishing"});
    
        page.bindingContext = viewModel;
    }
    
    export function listViewItemTap(args:ItemEventData) {
        var itemIndex = args.index;
    
        // example how to navigate details-page & pass the tapped item context
        // frameModule.topmost().navigate({
        //     moduleName: "./details-page",
        //     context: myItems.getItem(itemIndex);
        // });
    }



## Using ListView to display data (NativeScript + Angular-2)
*creating-listview.component.html*

    <ListView [items]="countries" (itemTap)="onItemTap($event)">
        <template let-country="item" let-i="index">
            <StackLayout orientation="horizontal">
                <Label [text]='(i + 1) +".) "' ></Label>
                <Label [text]='country.name'></Label>
            </StackLayout>
        </template>
    </ListView>

*creating-listview.component.ts*


    import { Component, ChangeDetectionStrategy, Input }  from "@angular/core";
    
    class Country {
        constructor(public name: string) { }
    }
    
    var europianCountries = ["Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czech Republic",
    "Denmark", "Estonia", "Finland", "France","Germany", "Greece", "Hungary", "Ireland", "Italy", 
    "Latvia", "Lithuania", "Luxembourg", "Malta", "Netherlands","Poland", "Portugal", "Romania", "Slovakia", 
    "Slovenia","Spain", "Sweden", "United Kingdom"];
                
    @Component({
        selector: "creating-listview",
        styleUrls:["./creating-listview.component.css"],
        templateUrl: "./creating-listview.component.html",
        changeDetection: ChangeDetectionStrategy.OnPush
    })

    export class CreatingListViewComponent {
        public countries: Array<Country>;
    
        constructor() {
            this.countries = [];
    
            for (var i = 0; i < europianCountries.length; i++) {
                this.countries.push(new Country(europianCountries[i]));
                }
        }
    
        public onItemTap(args) {
            console.log("Item Tapped at cell index: " + args.index);
        }
    }



## Using Repeater with Callbacks (JavaScript)
*page.js*

    var context = {
    items: [
            {id: 1, name: "Foo"},
            {id: 2, name: "Bar"},
            {id: 3, name: "Joe"}
        ]
    }
    
    exports.loaded = function(args){
        var page = args.object;
        page.bindingContext = context;
    }

    exports.showEntry = function(args){
        // select the tapped entry without passing an index or anything like that
        var selectedEntry = args.view.bindingContext;
        console.log(selectedEntry.id + " " + selectedEntry.name);
    }


*page.xml*

<Page loaded="loaded">
  
    <Repeater items="{{ items }}" >
        
        <Repeater.itemTemplate>
            <Label text="{{ name }}" tap="showEntry" />
        </Repeater.itemTemplate>
        
    </Repeater>
  
</Page>

