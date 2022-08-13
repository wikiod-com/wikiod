---
title: "Angular 2 Change detection and manual triggering"
slug: "angular-2-change-detection-and-manual-triggering"
draft: false
images: []
weight: 9986
type: docs
toc: true
---

## Basic example
Parent component :


----------

    import {Component} from '@angular/core';
    
    @Component({
      selector: 'parent-component',
      templateUrl: './parent-component.html'
    })
    export class ParentComponent {
      users : Array<User> = [];
      changeUsersActivation(user : User){
        user.changeButtonState();
      }
      constructor(){
        this.users.push(new User('Narco', false));
        this.users.push(new User('Bombasto',false));
        this.users.push(new User('Celeritas', false));
        this.users.push(new User('Magneta', false));
      }
    }
    
    
    export class User {
      firstName : string;
      active : boolean;
    
      changeButtonState(){
        this.active = !this.active;
      }
      constructor(_firstName :string, _active : boolean){
        this.firstName = _firstName;
        this.active = _active;
      }
    
    }

Parent HTML: 

    <div>
      <child-component [usersDetails]="users" 
                       (changeUsersActivation)="changeUsersActivation($event)">
       </child-component>
    </div>

child component :


----------

    import {Component, Input, EventEmitter, Output} from '@angular/core';
    import {User} from "./parent.component";
    
    @Component({
      selector: 'child-component',
      templateUrl: './child-component.html',
      styles: [`
         .btn {
          height: 30px;
          width: 100px;
          border: 1px solid rgba(0, 0, 0, 0.33);
          border-radius: 3px;
          margin-bottom: 5px;
         }
    
      `]
    })
    export class ChildComponent{
      @Input() usersDetails : Array<User> = null;
      @Output() changeUsersActivation = new EventEmitter();
    
      triggerEvent(user : User){
        this.changeUsersActivation.emit(user);
      }
    }


child HTML :


----------

 

    <div>
      <div>
        <table>
          <thead>
            <tr>
              <th>Name</th>
              <th></th>
            </tr>
          </thead>
          <tbody *ngIf="user !== null">
            <tr *ngFor="let user of usersDetails">
              <td>{{user.firstName}}</td>
              <td><button class="btn" (click)="triggerEvent(user)">{{user.active}}</button></td>
            </tr>
          </tbody>
        </table>
      </div>
    </div>




