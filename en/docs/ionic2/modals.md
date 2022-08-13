---
title: "Modals"
slug: "modals"
draft: false
images: []
weight: 9950
type: docs
toc: true
---

## Modal with parameters on create:
Passing parameters to a modal is similar to how we pass values to a NavController. To do so, we are altering our list in home.html to open a modal when clicking a list item and passing the required parameters as a second argument to the **create** method.

**Home.html**

     <ion-list>
        <ion-item *ngFor="let datum of data" (click)="openModalwithNavParams(datum)">
          <h1>{{datum.name}}</h1>
        </ion-item>
      </ion-list>

**Home.ts**

    import {EditProfilePage} from '../edit-profile/edit-profile';
    
      openModalwithNavParams(data){
        let modalWithNavParams = this.modalCtrl.create(EditProfilePage,{Data: data});
        modalWithNavParams.present();
      }

Similar to other views, we use NavParams to retrieve the data sent from the previous view.

**Edit-Profile.html**

    <ion-header>
      <ion-toolbar>
        <ion-title>
          Login
        </ion-title>
        <ion-buttons start>
          <button (click)="dismiss()">
            <span primary showWhen="ios">Cancel</span>
            <ion-icon name="md-close" showWhen="android,windows"></ion-icon>
          </button>
        </ion-buttons>
      </ion-toolbar>
    </ion-header>
    <ion-content padding>
      <h2>Welcome {{name}}</h2>
      <ion-list>
        <ion-item>
          <ion-label>Email</ion-label>
          <ion-input type="text" value={{email}}></ion-input>
        </ion-item>
        <ion-item>
          <ion-label>Mobile</ion-label>
          <ion-input type="number" value={{mobile}}></ion-input>
        </ion-item>
        <ion-item>
          <ion-label>Nickname</ion-label>
          <ion-input type="text" value={{nickname}}></ion-input>
        </ion-item>
      </ion-list>
      <button full (click)="dismiss()">Close</button>
    </ion-content>

**Edit-Profile.ts**

    import { Component } from '@angular/core';
    import { ViewController, NavParams } from 'ionic-angular';
    @Component({
      templateUrl: 'build/pages/edit-profile/edit-profile.html',
    })
    export class EditProfilePage {
      viewCtrl;
      navParams;
      data;
      name;
      email;
      mobile;
      nickname;
      constructor(viewCtrl: ViewController, navParams: NavParams) {
        this.viewCtrl = viewCtrl;
        this.navParams = navParams;
        this.data = this.navParams.get('Data');
        this.name = this.data.name;
        this.email = this.data.email;
        this.mobile = this.data.mobile;
        this.nickname = this.data.nickname;
        
      }
      dismiss(){
        this.viewCtrl.dismiss();
      }
    }



## Simple Modal
Modal is a temporary UI that is displayed on top of your current page. This is often used for login, signup, editing existing options and selecting options.  

Let us look in to a simple example with modals used. To begin with we are creating an ionic blank project. Let us create a simple modal displaying a message and exit on button click. To do that first we are creating view for our modal.

**Message.html**

    <ion-header>
      <ion-toolbar>
        <ion-title>
          Modal
        </ion-title>
        <ion-buttons start>
          <button (click)="dismiss()">
            <span primary showWhen="ios">Cancel</span>
            <ion-icon name="md-close" showWhen="android,windows"></ion-icon>
          </button>
        </ion-buttons>
      </ion-toolbar>
    </ion-header>
    <ion-content padding>
      <h1>Modal Without Params is created successfully.</h1>
      <button full (click)="dismiss()"> Exit </button>
    </ion-content>
    
**Message.ts**

    import { Component } from '@angular/core';
    import { ViewController } from 'ionic-angular';
    @Component({
      templateUrl: 'build/pages/message/message.html',
    })
    export class MessagePage {
      viewCtrl;
      constructor(viewCtrl: ViewController) {
        this.viewCtrl = viewCtrl;
      }
      dismiss(){
        this.viewCtrl.dismiss();
      }
    }


This modal displays a message. The modal can be closed or “dismissed” by using the View controllers **dismiss** method.


**Home.html**

    <ion-header>
      <ion-navbar>
        <ion-title>
          Modal Example
        </ion-title>
      </ion-navbar>
    </ion-header>
    <ion-content padding>
      <button full (click)="openModal()">ModalWithoutParams-Message</button>
    </ion-content>


**Home.ts**

    import { Component } from '@angular/core';
    import { ModalController } from 'ionic-angular';
    import {MessagePage} from '../message/message';
    @Component({
      templateUrl: 'build/pages/home/home.html'
    })
    export class HomePage {
      modalCtrl;
      data;
      constructor(modalCtrl: ModalController) {
        this.modalCtrl = modalCtrl;
        this.data = [{name: "aaa", email: "aaa.a@som.com", mobile: "1234567890", nickname: "zzz"},
          {name: "bbb", email: "bbb.a@som.com", mobile: "1234567890", nickname: "yyy"},
          {name: "ccc", email: "ccc.a@som.com", mobile: "1234567890", nickname: "xxx"}]
      }
      openModal() {
        let myModal = this.modalCtrl.create(MessagePage);
        myModal.present();
      }
    }

Now we are creating our home page importing the **ModalController** and our data model MessagePage. ModalController’s **create** method creates modal for our data model MessagePage that is saved to control variable myModal.  **Present** method opens the modal on top of our current page. 


## Modal with Parameters on dismiss:
We now know how to create a modal. But what if we want to pass some data from modal to our home page. To do so, let us look into an example with modal as Register page passing parameters to parent page.

**Register.html**

    <ion-header>
      <ion-toolbar>
        <ion-title>
          Login
        </ion-title>
        <ion-buttons start>
          <button (click)="dismiss()">
            <span primary showWhen="ios">Cancel</span>
            <ion-icon name="md-close" showWhen="android,windows"></ion-icon>
          </button>
        </ion-buttons>
      </ion-toolbar>
    </ion-header>
    <ion-content padding>
      <ion-list>
        <ion-item>
          <ion-label>Name</ion-label>
          <ion-input type="text" [(ngModel)]="name"></ion-input>
        </ion-item>
        <ion-item>
          <ion-label>Email</ion-label>
          <ion-input type="text" [(ngModel)]="email"></ion-input>
        </ion-item>
        <ion-item>
          <ion-label>Mobile</ion-label>
          <ion-input type="number" [(ngModel)]="mobile"></ion-input>
        </ion-item>
        <ion-item>
          <ion-label>Nickname</ion-label>
          <ion-input type="text" [(ngModel)]="nickname"></ion-input>
        </ion-item>
      </ion-list>
      <button full (click)="add()">Add</button>
    </ion-content>

**Register.ts**

    import { Component } from '@angular/core';
    import { ViewController } from 'ionic-angular';
    @Component({
      templateUrl: 'build/pages/register/register.html',
    })
    export class ResisterPage {
      viewCtrl;
      name;
      email;
      mobile;
      nickname;
      constructor(viewCtrl: ViewController) {
        this.viewCtrl = viewCtrl;
        this.name = "";
        this.email = "";
        this.mobile = "";
        this.nickname = "";
      }
      dismiss(){
        this.viewCtrl.dismiss();
      }
      add(){
        let data = {"name": this.name, "email": this.email, "mobile": this.mobile, "nickname": this.nickname};
        this.viewCtrl.dismiss(data);
      }
    }

　Register modal gets data object with values entered by user and the parameters are passed to our current page on dismiss with viewControllers dismiss method. Now the parameters are sent.

　So how we are going to retrieve the parameters in home page? To do so, we are creating a button on home page and call Register modal on click. To display the user, we are displaying a list.

　
　**Home.html**

    <ion-list>
      <ion-item *ngFor="let datum of data">
        <h1>{{datum.name}}</h1>
      </ion-item>
    </ion-list>
    <button full secondary (click)="openModalParams()">ModalWithParams-Register</button>

　**Home.ts**

    import {ResisterPage} from '../register/register';

      openModalParams(){
        let modalWithParams = this.modalCtrl.create(ResisterPage);
        modalWithParams.present();
    
        modalWithParams.onDidDismiss((result) =>{
          if(result){
            this.data.unshift(result);
          }
        });
      }

ViewController **onDidDismiss** method gets executed whenever a modal is closed. If data is passed as parameter from modal, then we can retrieve it using onDidDismiss method. Here the data entered by user is appended to the existing data. If no data is passed as parameter, then the returned value will be null. 

