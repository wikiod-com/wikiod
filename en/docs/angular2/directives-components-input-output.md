---
title: "Directives & components  @Input @Output"
slug: "directives--components--input-output"
draft: false
images: []
weight: 9828
type: docs
toc: true
---

## Syntax
 1. One way binding from parent component to nested component: [propertyName]
 2. One way binding from nested component to parent component: (propertyName)
 3. Two-way binding (a.k.a banana box notation) : [(propertyName)]

## Angular2 @Input and @Output in a nested component


## Input example

@input is useful to bind data between components

First, import it in your component

<!-- language: lang-ts -->

    import { Input } from '@angular/core';

Then, add the input as a property of your component class

<!-- language: lang-ts -->

    @Input() car: any;

Let's say that the selector of your component is 'car-component', when you call the component, add the attribute 'car' 

<!-- language: lang-html -->

    <car-component [car]="car"></car-component>

Now your car is accessible as an attribute in your object (this.car)

Full Example :

 1. car.entity.ts

<!-- language: lang-ts -->

        export class CarEntity {
           constructor(public brand : string, public color : string) {
           }  
        }

 2. car.component.ts

<!-- language: lang-ts -->

        import { Component, Input } from '@angular/core';
        import {CarEntity} from "./car.entity";
        
        @Component({
            selector: 'car-component',
            template: require('./templates/car.html'),
        })
        
        export class CarComponent {
            @Input() car: CarEntity;
        
            constructor() {
                console.log('gros');
            }
        }

 3. garage.component.ts

<!-- language: lang-ts -->

        import { Component } from '@angular/core';
        import {CarEntity} from "./car.entity";
        import {CarComponent} from "./car.component";
        
        @Component({
            selector: 'garage',
            template: require('./templates/garage.html'),
            directives: [CarComponent]
        })
        
        export class GarageComponent {
            public cars : Array<CarEntity>;
        
            constructor() {
                var carOne : CarEntity = new CarEntity('renault', 'blue');
                var carTwo : CarEntity = new CarEntity('fiat', 'green');
                var carThree : CarEntity = new CarEntity('citroen', 'yellow');
                this.cars = [carOne, carTwo, carThree];
            }
        }
    

 4. garage.html

<!-- language: lang-html -->

        <div *ngFor="let car of cars">
        <car-component [car]="car"></car-component>
        </div>

 5. car.html

<!-- language: lang-html -->

        <div>
            <span>{{ car.brand }}</span> |
            <span>{{ car.color }}</span>
        </div>

 

## Angular2 @Input with asynchronous data
Sometimes you need to fetch data asynchronously before passing it to a child component to use. If the child component tries to use the data before it has been received, it will throw an error. You can use `ngOnChanges` to detect changes in a components' `@Input`s and wait until they are defined before acting upon them.

# Parent component with async call to an endpoint

    import { Component, OnChanges, OnInit } from '@angular/core';
    import { Http, Response } from '@angular/http';
    import { ChildComponent } from './child.component';

    @Component ({
        selector : 'parent-component',
        template : `
            <child-component [data]="asyncData"></child-component>
        `
    })
    export class ParentComponent {
        
        asyncData : any;

        constructor(
            private _http : Http
        ){}

        ngOnInit () {
            this._http.get('some.url')
                .map(this.extractData)
                .subscribe(this.handleData)
                .catch(this.handleError);
        }

        extractData (res:Response) {
            let body = res.json();
            return body.data || { };
        }

        handleData (data:any) {
            this.asyncData = data;
        }

        handleError (error:any) {
            console.error(error);
        }
    }


# Child component which has async data as input

This child component takes the async data as input. Therefore it must wait for the data to exist before Using it. We use ngOnChanges which fires whenever a component's input changes, check if the data exists and use it if it does. Notice that the template for the child  will not show if a property that relies on the data being passed in is not true.

    import { Component, OnChanges, Input } from '@angular/core';

    @Component ({
        selector : 'child-component',
        template : `
            <p *ngIf="doesDataExist">Hello child</p>
        `
    })
    export class ChildComponent {
        
        doesDataExist: boolean = false;

        @Input('data') data : any;

        // Runs whenever component @Inputs change
        ngOnChanges () {
            // Check if the data exists before using it
            if (this.data) {
                this.useData(data);
            {
        }

        // contrived example to assign data to reliesOnData    
        useData (data) {
            this.doesDataExist = true; 
        }
    }

 

