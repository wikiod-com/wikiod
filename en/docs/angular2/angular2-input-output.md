---
title: "Angular2 Input() output()"
slug: "angular2-input-output"
draft: false
images: []
weight: 9973
type: docs
toc: true
---

## Input()

Parent Component : Initialize users lists.
------------------------------------------


    @Component({
      selector: 'parent-component',
      template: '<div>
                    <child-component [users]="users"></child-component>
                 </div>'
    })
    export class ParentComponent implements OnInit{
      let users : List<User> = null;
      
      ngOnInit() {
        users.push(new User('A', 'A', 'A@gmail.com');
        users.push(new User('B', 'B', 'B@gmail.com'); 
        users.push(new User('C', 'C', 'C@gmail.com');  
      }      
    }
    


Child component get user from parent component with Input()

----------


    @Component({
    selector: 'child-component',
      template: '<div>
                      <table *ngIf="users !== null">
                        <thead>
                             <th>Name</th>
                             <th>FName</th>
                             <th>Email</th>   
                        </thead>
                        <tbody>
                            <tr *ngFor="let user of users">
                                <td>{{user.name}}</td>
                                <td>{{user.fname}}</td>
                                <td>{{user.email}}</td>
                            </tr>
                        </tbody>
                      </table>
                    
                 </div>',
    })
    export class ChildComponent {
      @Input() users : List<User> = null;
    }


    export class User {
      name : string;
      fname : string;
      email : string;
    
      constructor(_name : string, _fname : string, _email : string){
         this.name = _name;
         this.fname = _fname;
         this.email = _email;
      }
    }

## Simple example of Input Properties
Parent element html

    <child-component [isSelected]="inputPropValue"></child-component>

Parent element ts
    
    export class AppComponent {
         inputPropValue: true
    }

Child component ts:
    
    export class ChildComponent {
        @Input() inputPropValue = false;
    }

Child component html:

    <div [class.simpleCssClass]="inputPropValue"></div>

This code will send the inputPropValue from the parent component to the child and it will have the value we have set in the parent component when it arrives there - false in our case. We can then use that value in the child component to, for example add a class to an element.

