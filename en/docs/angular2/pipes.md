---
title: "Pipes"
slug: "pipes"
draft: false
images: []
weight: 9436
type: docs
toc: true
---

The pipe `|` character is used to apply pipes in Angular 2. Pipes are very similar to filters in AngularJS in that they both help to transform the data into a specified format.

## Parameters
|Function/Parameter| Explanation|
|--------|--------|
| **@Pipe({name, pure})** | metadata for pipe, must immediately precede pipe class
| name: *string* | what you will use inside the template |
| pure: *boolean* | defaults to true, mark this as false to have your pipe re-evaluated more often|
| **transform( value, args[]? )** | the function that is called to transform the values in the template |
| value: *any* | the value that you want to transform |
| args: *any[]* | the arguments that you may need included in your transform. Mark optional args with the ? operator like so transform(value, arg1, arg2?) |

This topic covers [Angular2 Pipes][1], a mechanism for transforming and formatting data within HTML templates in an Angular2 application.


  [1]: https://angular.io/docs/ts/latest/guide/pipes.html

## Custom Pipes
*my.pipe.ts*

<!-- language: lang-js -->

    import { Pipe, PipeTransform } from '@angular/core';
    
    @Pipe({name: 'myPipe'})
    export class MyPipe implements PipeTransform {

      transform(value:any, args?: any):string {
        let transformedValue = value; // implement your transformation logic here
        return transformedValue;
      }

    }

*my.component.ts*

<!-- language: lang-js -->

    import { Component } from '@angular/core';
    
    @Component({
      selector: 'my-component',
      template: `{{ value | myPipe }}`
    })
    export class MyComponent {
    
        public value:any;
    
    }

*my.module.ts*

<!-- language: lang-js -->
    import { NgModule } from '@angular/core';
    import { BrowserModule } from '@angular/platform-browser';

    import { MyComponent } from './my.component';
    import { MyPipe } from './my.pipe';

    @NgModule({
      imports: [
        BrowserModule,
      ],
      declarations: [
        MyComponent,
        MyPipe
      ],
    })
    export class MyModule { }


## Built-in Pipes
## Angular2 comes with a few built-in pipes:


|Pipe|Usage|Example|
|--- |---|-----|
|[`DatePipe`][1]|`date`|<code>{{ dateObj \| date }} // output is 'Jun 15, 2015'</code>
[`UpperCasePipe`][2]|`uppercase`|<code>{{ value \| uppercase }} // output is 'SOMETEXT'</code>
[`LowerCasePipe`][3]|`lowercase`|<code>{{ value \| lowercase }} // output is 'sometext'</code>
[`CurrencyPipe`][4]|`currency`|<code>{{ 31.00 \| currency:'USD':true }} // output is '$31'</code>
[`PercentPipe`][5]|`percent`|<code>{{ 0.03 \| percent }} //output is %3</code>

There are others. Look [here][6] for their documentation.

# Example
### hotel-reservation.component.ts
<!-- language: lang-js -->
    import { Component } from '@angular/core';
    
    @Component({
        moduleId: module.id,
        selector: 'hotel-reservation',
        templateUrl: './hotel-reservation.template.html'
    })
    export class HotelReservationComponent {
        public fName: string =  'Joe';
        public lName: string = 'SCHMO';
        public reservationMade: string = '2016-06-22T07:18-08:00'
        public reservationFor: string = '2025-11-14';
        public cost: number = 99.99;
    }

### hotel-reservation.template.html
<!-- language: lang-html -->
    <div>
        <h1>Welcome back {{fName | uppercase}} {{lName | lowercase}}</h1>
        <p>
            On {reservationMade | date} at {reservationMade | date:'shortTime'} you 
            reserved room 205 for {reservationDate | date} for a total cost of 
            {cost | currency}.
        </p>
    </div>

## Output
    Welcome back JOE schmo
    On Jun 26, 2016 at 7:18 you reserved room 205 for Nov 14, 2025 for a total cost of 
    $99.99.


  [1]: https://angular.io/docs/ts/latest/api/common/index/DatePipe-pipe.html
  [2]: https://angular.io/docs/ts/latest/api/common/index/UpperCasePipe-pipe.html
  [3]: https://angular.io/docs/ts/latest/api/common/index/LowerCasePipe-pipe.html
  [4]: https://angular.io/docs/ts/latest/api/common/index/CurrencyPipe-pipe.html
  [5]: https://angular.io/docs/ts/latest/api/common/index/PercentPipe-pipe.html
  [6]: https://angular.io/docs/ts/latest/api/#!?apiFilter=pipe

## Chaining Pipes
Pipes may be chained.

<!-- language: lang-html -->

    <p>Today is {{ today | date:'fullDate' | uppercase}}.</p>

## Debugging With JsonPipe
The JsonPipe can be used for debugging the state of any given internal.  

## Code

<!-- language: lang-js -->

    @Component({
      selector: 'json-example',
      template: `<div>
        <p>Without JSON pipe:</p>
        <pre>{{object}}</pre>
        <p>With JSON pipe:</p>
        <pre>{{object | json}}</pre>
      </div>`
    })
    export class JsonPipeExample {
      object: Object = {foo: 'bar', baz: 'qux', nested: {xyz: 3, numbers: [1, 2, 3, 4, 5]}};
    }

## Output

    Without JSON Pipe:
    object
    With JSON pipe:
    {object:{foo: 'bar', baz: 'qux', nested: {xyz: 3, numbers: [1, 2, 3, 4, 5]}}

## Dynamic Pipe
Use case scenario:
A table view consists of different columns with different data format that needs to be transformed with different pipes.

*table.component.ts*
```javascript
...
import { DYNAMIC_PIPES } from '../pipes/dynamic.pipe.ts';

@Component({
    ...
    pipes: [DYNAMIC_PIPES]
})
export class TableComponent {
    ...

    // pipes to be used for each column
    table.pipes = [ null, null, null, 'humanizeDate', 'statusFromBoolean' ],
    table.header = [ 'id', 'title', 'url', 'created', 'status' ],
    table.rows = [
        [ 1, 'Home', 'home', '2016-08-27T17:48:32', true ],
        [ 2, 'About Us', 'about', '2016-08-28T08:42:09', true ],
        [ 4, 'Contact Us', 'contact', '2016-08-28T13:28:18', false ],
        ...
    ]
    ...

}
```


*dynamic.pipe.ts*
```javascript
import {
    Pipe,
    PipeTransform
} from '@angular/core';
// Library used to humanize a date in this example
import * as moment from 'moment';

@Pipe({name: 'dynamic'})
export class DynamicPipe implements PipeTransform {

    transform(value:string, modifier:string) {
        if (!modifier) return value;
        // Evaluate pipe string
        return eval('this.' + modifier + '(\'' + value + '\')')
    }

    // Returns 'enabled' or 'disabled' based on input value
    statusFromBoolean(value:string):string {
        switch (value) {
            case 'true':
            case '1':
                return 'enabled';
            default:
                return 'disabled';
        }
    }

    // Returns a human friendly time format e.g: '14 minutes ago', 'yesterday'
    humanizeDate(value:string):string {
        // Humanize if date difference is within a week from now else returns 'December 20, 2016' format
        if (moment().diff(moment(value), 'days') < 8) return moment(value).fromNow();
        return moment(value).format('MMMM Do YYYY');
    }
}

export const DYNAMIC_PIPES = [DynamicPipe];
```

*table.component.html*
```html
<table>
    <thead>
        <td *ngFor="let head of data.header">{{ head }}</td>
    </thead>
    <tr *ngFor="let row of table.rows; let i = index">
        <td *ngFor="let column of row">{{ column | dynamic:table.pipes[i] }}</td>
    </tr>
</table>
```


*Result*
```html
| ID | Page Title     | Page URL    | Created          | Status     |
---------------------------------------------------------------------
|  1 | Home           | home        | 4 minutes ago    | Enabled    |
|  2 | About Us       | about       | Yesterday        | Enabled    |
|  4 | Contact Us     | contact     | Yesterday        | Disabled   |
---------------------------------------------------------------------
```





## Unwrap async values with async pipe
<!-- language: lang-js -->


    import { Component } from '@angular/core';
    import { Observable } from 'rxjs/Observable';
    import 'rxjs/add/observable/of';
    
    @Component({
      selector: 'async-stuff',
      template: `
        <h1>Hello, {{ name | async }}</h1>
        Your Friends are:
        <ul>
          <li *ngFor="let friend of friends | async">
            {{friend}}
          </li>
        </ul>
      `
    })
    class AsyncStuffComponent {
      name = Promise.resolve('Misko');
      friends = Observable.of(['Igor']);
    }


Becomes:

<!-- language: lang-html --> 


    <h1>Hello, Misko</h1>
    Your Friends are:
    <ul>
      <li>
        Igor
      </li>
    </ul>

## Stateful Pipes
Angular 2 offers two different types of pipes - stateless and stateful. Pipes are stateless by default. However, we can implement stateful pipes by setting the `pure` property to `false`. As you can see in the parameter section, you can specify a `name` and declare whether the pipe should be pure or not, meaning stateful or stateless. While data flows through a stateless pipe (which is a pure function) that **does not** remember anything, data can be managed and remembered by stateful pipes. A good example of a stateful pipe is the `AsyncPipe` that is provided by Angular 2.

**Important**

Notice that most pipes should fall into the category of stateless pipes. That's important for performance reasons since Angular can optimize stateless pipes for the change detector. So use stateful pipes cautiously. In general, the optimization of pipes in Angular 2 have a major performance enhancement over filters in Angular 1.x. In Angular 1 the digest cycle always had to re-run all filters even though the data hasn't changed at all. In Angular 2, once a pipe's value has been computed, the change detector knows not to run this pipe again unless the input changes. 

**Implementation of a stateful pipe**

    import {Pipe, PipeTransform, OnDestroy} from '@angular/core';
    
    @Pipe({
      name: 'countdown',
      pure: false
    })
    export class CountdownPipe implements PipeTransform, OnDestroy  {
      private interval: any;
      private remainingTime: number;
    
      transform(value: number, interval: number = 1000): number {
        if (!parseInt(value, 10)) {
          return null;
        }
        
        if (typeof this.remainingTime !== 'number') {
          this.remainingTime = parseInt(value, 10);
        }
        
        if (!this.interval) {
          this.interval = setInterval(() => {
            this.remainingTime--;
            
            if (this.remainingTime <= 0) {
              this.remainingTime = 0;
              clearInterval(this.interval);
              delete this.interval;
            }
          }, interval);
        }
        
        return this.remainingTime;
      }
      
      ngOnDestroy(): void {
        if (this.interval) {
          clearInterval(this.interval);
        }
      }
    }

You can then use the pipe as usual:

    {{ 1000 | countdown:50 }}
    {{ 300 | countdown }}

It's important that your pipe also implements the `OnDestroy` interface so you can clean up once your pipe gets destroyed. In the example above, it's necessary to clear the interval to avoid memory leaks.

## Creating Custom Pipe


## Globally Available Custom Pipe
To make a custom pipe available application wide, During application bootstrap, extending PLATFORM_PIPES.

<!-- language: lang-js -->

    import { bootstrap }    from '@angular/platform-browser-dynamic';
    import { provide, PLATFORM_PIPES } from '@angular/core';
    
    import { AppComponent } from './app.component';
    import { MyPipe } from './my.pipe'; // your custom pipe
    
    bootstrap(AppComponent, [
      provide(PLATFORM_PIPES, {
                useValue: [
                    MyPipe 
                ],
                multi: true
            })
    ]);


Tutorial here: https://scotch.io/tutorials/create-a-globally-available-custom-pipe-in-angular-2

## Extending an Existing Pipe
<!-- language: lang-js --> 

    import { Pipe, PipeTransform } from '@angular/core';
    import { DatePipe } from '@angular/common'
    
    
    @Pipe({name: 'ifDate'})
    export class IfDate implements PipeTransform {
      private datePipe: DatePipe = new DatePipe();
    
      transform(value: any, pattern?:string) : any {
        if (typeof value === 'number') {return value}
        try {
          return this.datePipe.transform(value, pattern)
        } catch(err) {
          return value
        }
      }
    }



## Testing a pipe
Given a pipe that reverse a string

    import { Pipe, PipeTransform } from '@angular/core';
    
    @Pipe({ name: 'reverse' })
    export class ReversePipe implements PipeTransform {
      transform(value: string): string {
        return value.split('').reverse().join('');
      }
    }

It can be tested configuring the spec file like this

    import { TestBed, inject } from '@angular/core/testing';
    
    import { ReversePipe } from './reverse.pipe';
    
    describe('ReversePipe', () => {
      beforeEach(() => {
        TestBed.configureTestingModule({
          providers: [ReversePipe],
        });
      });
    
      it('should be created', inject([ReversePipe], (reversePipe: ReversePipe) => {
        expect(reversePipe).toBeTruthy();
      }));
    
      it('should reverse a string', inject([ReversePipe], (reversePipe: ReversePipe) => {
        expect(reversePipe.transform('abc')).toEqual('cba');
      }));
    });



