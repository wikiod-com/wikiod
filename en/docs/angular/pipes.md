---
title: "Pipes"
slug: "pipes"
draft: false
images: []
weight: 9984
type: docs
toc: true
---

Pipes are very similar to filters in AngularJS in that they both help to transform the data into a specified format.The pipe character `|` is used to apply pipes in Angular.

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


## Multiple custom pipes
Having different pipes is a very common case, where each pipe does a different thing. Adding each pipe to each component may become a repetitive code. 

It is possible to bundle all frequently used pipes in one `Module` and import that new module in any component needs the pipes.

*breaklines.ts*


<!-- language: lang-js -->

    import { Pipe } from '@angular/core';
    /**
     * pipe to convert the \r\n into <br />
     */
    @Pipe({ name: 'br' })
    export class BreakLine {
        transform(value: string): string {
            return value == undefined ? value : 
                 value.replace(new RegExp('\r\n', 'g'), '<br />')
                  .replace(new RegExp('\n', 'g'), '<br />');
        }
    }


*uppercase.ts*
<!-- language: lang-js -->

    import { Pipe } from '@angular/core';
    /**
     * pipe to uppercase a string
     */
    @Pipe({ name: 'upper' })
    export class Uppercase{
        transform(value: string): string {
            return value == undefined ? value : value.toUpperCase( );
        }
    }

*pipes.module.ts*

<!-- language: lang-js -->

    import { NgModule } from '@angular/core';
    import { BreakLine } from './breakLine';
    import { Uppercase} from './uppercase';
    
    @NgModule({
        declarations: [
            BreakLine,
            Uppercase
        ],
        imports: [
    
        ],
        exports: [
            BreakLine,
            Uppercase
        ]
        ,
    })
    export class PipesModule {}

*my.component.ts*
<!-- language: lang-js -->

    import { Component } from '@angular/core';
    
    @Component({
      selector: 'my-component',
      template: `{{ value | upper | br}}`
    })
    export class MyComponent {
    
        public value: string;
    
    }

*my.module.ts*
<!-- language: lang-js -->

    import { NgModule } from '@angular/core';
    import { BrowserModule } from '@angular/platform-browser';
    
    import { MyComponent } from './my.component';
    import { PipesModule} from './pipes.module';
    
    @NgModule({
      imports: [
        BrowserModule,
        PipesModule,
      ],
      declarations: [
        MyComponent,
      ],
    })





