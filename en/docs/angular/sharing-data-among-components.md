---
title: "Sharing data among components"
slug: "sharing-data-among-components"
draft: false
images: []
weight: 9977
type: docs
toc: true
---

The objective of this topic is to create simple examples of several ways data can be shared between components via data binding and shared service.

There are always many of ways of accomplishing one task in programming. Please feel free to edit current examples or add some of your own. 

## Sending data from child to parent via @Output event emitter
**event-emitter.component.ts**

<!-- language: typescript -->

    import { Component, OnInit, EventEmitter, Output } from '@angular/core';
    
    @Component({
      selector: 'event-emitting-child-component',
      template: `<div *ngFor="let item of data">
                   <div (click)="select(item)">
                     {{item.id}} = {{ item.name}}
                   </div>
                 </div>
                `
    })

    export class EventEmitterChildComponent implements OnInit{
      
      data;
      
      @Output()
      selected: EventEmitter<string> = new EventEmitter<string>();
      
      ngOnInit(){
        this.data = [ { "id": 1, "name": "Guy Fawkes", "rate": 25 }, 
                      { "id": 2, "name": "Jeremy Corbyn", "rate": 20 }, 
                      { "id": 3, "name": "Jamie James", "rate": 12 }, 
                      { "id": 4, "name": "Phillip Wilson", "rate": 13 }, 
                      { "id": 5, "name": "Andrew Wilson", "rate": 30 }, 
                      { "id": 6, "name": "Adrian Bowles", "rate": 21 }, 
                      { "id": 7, "name": "Martha Paul", "rate": 19 }, 
                      { "id": 8, "name": "Lydia James", "rate": 14 }, 
                      { "id": 9, "name": "Amy Pond", "rate": 22 }, 
                      { "id": 10, "name": "Anthony Wade", "rate": 22 } ]
      }
      
      select(item) { 
          this.selected.emit(item);
            
      }
    
    }

**event-receiver.component.ts:**

<!-- language: typescript -->

    import { Component } from '@angular/core';
    
    @Component({
        selector: 'event-receiver-parent-component',
        template: `<event-emitting-child-component (selected)="itemSelected($event)">
                    </event-emitting-child-component>
                    <p *ngIf="val">Value selected</p>
                    <p style="background: skyblue">{{ val | json}}</p>`
    })
    
    export class EventReceiverParentComponent{ 
      val;
      
      itemSelected(e){
        this.val = e;
      }
    }



## Sending data from parent component to child via shared service
**service.ts:**

<!-- language: typescript -->

    import { Injectable } from '@angular/core';
    
    @Injectable()
    export class AppState {
      
      public mylist = [];
      
    }

**parent.component.ts:**

<!-- language: typescript -->

    import {Component} from '@angular/core';
    import { AppState } from './shared.service';
    
    @Component({
      selector: 'parent-example',
      templateUrl: 'parent.component.html',
    })
    export class ParentComponent {
      mylistFromParent = [];
      
      constructor(private appState: AppState){
        this.appState.mylist;
      }
      
      add() {
        this.appState.mylist.push({"itemName":"Something"});  
      }
      
    }

**parent.component.html:**

<!-- language: lang-html -->

    <p> Parent </p>
      <button (click)="add()">Add</button>
    <div>
      <child-component></child-component>
    </div>

**child.component.ts:**

<!-- language: typescript -->

    import {Component, Input } from '@angular/core';
    import { AppState } from './shared.service';
    
    @Component({
      selector: 'child-component',
      template: `
        <h3>Child powered by shared service</h3>
            {{mylist | json}}
      `,
    })
    export class ChildComponent {
      mylist: any;
      
      constructor(private appState: AppState){
        this.mylist = this.appState.mylist;
      }
    
    }

## Send data from parent component to child component via data binding using @Input
**parent.component.ts:**

<!-- language: typescript -->

    import {Component} from '@angular/core';
    
    @Component({
      selector: 'parent-example',
      templateUrl: 'parent.component.html',
    })
    
    export class ParentComponent {
      mylistFromParent = [];
      
      add() {
        this.mylistFromParent.push({"itemName":"Something"}); 
      }
      
    }

**parent.component.html:**

<!-- language: lang-html -->

    <p> Parent </p>
      <button (click)="add()">Add</button>
    
    <div>
      <child-component [mylistFromParent]="mylistFromParent"></child-component>
    </div>

**child.component.ts:**

<!-- language: typescript -->

    import {Component, Input } from '@angular/core';
    
    @Component({
      selector: 'child-component',
      template: `
        <h3>Child powered by parent</h3>
            {{mylistFromParent | json}}
      `,
    })
    
    export class ChildComponent {
      @Input() mylistFromParent = [];
    }

## Sending data asynchronous from parent to child using Observable and Subject
**shared.service.ts:**

<!-- language: typescript -->

    import { Injectable }    from '@angular/core';
    import { Headers, Http } from '@angular/http';
    
    import 'rxjs/add/operator/toPromise';
    
    import { Observable } from 'rxjs/Observable';
    import { Observable } from 'rxjs/Rx';
    import {Subject} from 'rxjs/Subject';
    
    
    @Injectable()
    export class AppState {
      
      private headers = new Headers({'Content-Type': 'application/json'});
      private apiUrl = 'api/data';
      
      // Observable string source
      private dataStringSource = new Subject<string>();
    
      // Observable string stream
      dataString$ = this.dataStringSource.asObservable();
      
      constructor(private http: Http) { }
      
      public setData(value) {
        this.dataStringSource.next(value);
      }
      
      fetchFilterFields() {
        console.log(this.apiUrl);
        return this.http.get(this.apiUrl)
                   .delay(2000)
                   .toPromise()
                   .then(response => response.json().data)
                   .catch(this.handleError);
      }
      
      private handleError(error: any): Promise<any> {
        console.error('An error occurred', error); // for demo purposes only
        return Promise.reject(error.message || error);
      }
    
    }

**parent.component.ts:**

<!-- language: typescript -->

    import {Component, OnInit} from '@angular/core';
    import 'rxjs/add/operator/toPromise';
    import { AppState } from './shared.service';
    
    @Component({
      selector: 'parent-component',
      template: `
                  <h2> Parent </h2>
                  <h4>{{promiseMarker}}</h4>

                  <div>
                    <child-component></child-component>
                  </div>
                `
    })
    export class ParentComponent implements OnInit {
      
      promiseMarker = "";
      
      constructor(private appState: AppState){ }
      
      ngOnInit(){
        this.getData();
      }
      
      getData(): void {
        this.appState
            .fetchFilterFields()
            .then(data => {
              // console.log(data)
              this.appState.setData(data);
              this.promiseMarker = "Promise has sent Data!";
            });
      }
      
    }

**child.component.ts:**

<!-- language: typescript -->

    import {Component, Input } from '@angular/core';
    import { AppState } from './shared.service';
    
    
    @Component({
      selector: 'child-component',
      template: `
        <h3>Child powered by shared service</h3>
            {{fields | json}}
      `,
    })
    export class ChildComponent {
      fields: any;
      
      constructor(private appState: AppState){
        // this.mylist = this.appState.get('mylist');
      
        this.appState.dataString$.subscribe(
          data => {
            // console.log("Subs to child" + data);
            this.fields = data; 
          });
        
      }
    
    }

