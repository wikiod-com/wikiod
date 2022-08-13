---
title: "Angular - ForLoop"
slug: "angular---forloop"
draft: false
images: []
weight: 9899
type: docs
toc: true
---

## Syntax
 1. < div *ngFor="let item of items; let i = index">{{i}} {{item}}</ div>

The `*ngFor` structural directive runs as a loop in a collection and repeats a piece of html for each element of a collection.

`@View` decorator is now deprecated. Developers should be using `template` or 'templateUrl' properties for `@Component` decorator.

## NgFor - Markup For Loop
The **NgFor** directive instantiates a template once per item from an iterable. The context for each instantiated template inherits from the outer context with the given loop variable set to the current item from the iterable.

To customize the default tracking algorithm, NgFor supports **trackBy** option. **trackBy** takes a function which has two arguments: index and item. If **trackBy** is given, Angular tracks changes by the return value of the function.

    <li *ngFor="let item of items; let i = index; trackBy: trackByFn">
        {{i}} - {{item.name}}
    </li>

**Additional Options**: 
NgFor provides several exported values that can be aliased to local variables:

 - **index** will be set to the current loop iteration for each template context.
 - **first** will be set to a boolean value indicating whether the item is the first one in the iteration.
 - **last** will be set to a boolean value indicating whether the item is the last one in the iteration.
 - **even** will be set to a boolean value indicating whether this item has an even index.
 - **odd** will be set to a boolean value indicating whether this item has an odd index.

## *ngFor with component
   

       @Component({
         selector: 'main-component',
         template: '<example-component    
                       *ngFor="let hero of heroes"
                       [hero]="hero"></example-component>'
       })


       @Component({
          selector: 'example-component',
          template: '<div>{{hero?.name}}</div>'
       })

       export class ExampleComponent {
         @Input() hero : Hero = null;
       }

 




## Angular 2 for-loop
For live [plnkr click...][1]

    <!doctype html>
    <html>
    <head>
        <title>ng for loop in angular 2 with ES5.</title>
        <script type="text/javascript" src="https://code.angularjs.org/2.0.0-alpha.28/angular2.sfx.dev.js"></script>
        <script>
            var ngForLoop = function () {
                this.msg = "ng for loop in angular 2 with ES5.";
                this.users = ["Anil Singh", "Sunil Singh", "Sushil Singh", "Aradhya", 'Reena'];
            };
    
            ngForLoop.annotations = [
                    new angular.Component({
                        selector: 'ngforloop'
                    }),
                    new angular.View({
                        template: '<H1>{{msg}}</H1>' +
                                '<p> User List : </p>' +
                                '<ul>' +
                                '<li *ng-for="let user of users">' +
                                '{{user}}' +
                                '</li>' +
                                '</ul>',
                        directives: [angular.NgFor]
                    })
            ];
    
            document.addEventListener("DOMContentLoaded", function () {
                angular.bootstrap(ngForLoop);
            });
        </script>
    </head>
    <body>
        <ngforloop></ngforloop>
        <h2>
          <a href="http://www.code-sample.com/" target="_blank">For more detail...</a>
        </h2>
    </body>
    </html>


  [1]: http://embed.plnkr.co/5btVMZ/preview

## *ngFor X amount of items per row
Example shows 5 items per row:

    <div *ngFor="let item of items; let i = index">
      <div *ngIf="i % 5 == 0" class="row">
        {{ item }}
        <div *ngIf="i + 1 < items.length">{{ items[i + 1] }}</div>
        <div *ngIf="i + 2 < items.length">{{ items[i + 2] }}</div>
        <div *ngIf="i + 3 < items.length">{{ items[i + 3] }}</div>
        <div *ngIf="i + 4 < items.length">{{ items[i + 4] }}</div>
      </div>
    </div>

## *ngFor in the Table Rows


    <table>
        <thead>
            <th>Name</th>
            <th>Index</th>
        </thead>
        <tbody>
            <tr *ngFor="let hero of heroes">
                <td>{{hero.name}}</td>
            </tr>
        </tbody>
    </table>

