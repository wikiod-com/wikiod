---
title: "How to use ngfor"
slug: "how-to-use-ngfor"
draft: false
images: []
weight: 9910
type: docs
toc: true
---

The `ngFor` directive is used by Angular2 to instantiate a template once for every item in an iterable object. This directive binds the iterable to the DOM, so if the content of the iterable changes, the content of the DOM will be also changed.

## *ngFor with pipe
    
    import { Pipe, PipeTransform } from '@angular/core';
    @Pipe({
      name: 'even'
    })

    export class EvenPipe implements PipeTransform {
        transform(value: string): string {
            if(value && value %2 === 0){
              return value;
            }
        }
    }

    @Component({
          selector: 'example-component',
          template: '<div>
                          <div *ngFor="let number of numbers | even">
                              {{number}}
                          </div>
                    </div>'
    })

    export class exampleComponent {
        let numbers : List<number> = Array.apply(null, {length: 10}).map(Number.call, Number)
    }

## Unordered list example
    <ul>
      <li *ngFor="let item of items">{{item.name}}</li>
    </ul>

## More complext template example
    <div *ngFor="let item of items">
      <p>{{item.name}}</p>
      <p>{{item.price}}</p>
      <p>{{item.description}}</p>
    </div>

## Tracking current interaction example
    <div *ngFor="let item of items; let i = index">
      <p>Item number: {{i}}</p>    
    </div>

In this case, i will take the value of index, which is the current loop iteration.

## Angular2 aliased exported values
Angular2 provides several exported values that can be aliased to local variables. These are:
- index
- first
- last
- even
- odd

Except `index`, the other ones take a `Boolean` value. As the previous example using index, it can be used any of these exported values:

    <div *ngFor="let item of items; let firstItem = first; let lastItem = last">
      <p *ngIf="firstItem">I am the first item and I am gonna be showed</p>
      <p *ngIf="firstItem">I am not the first item and I will not show up :(</p>
      <p *ngIf="lastItem">But I'm gonna be showed as I am the last item :)</p>
    </div>

