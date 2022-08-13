---
title: "Forms"
slug: "forms"
draft: false
images: []
weight: 9983
type: docs
toc: true
---

## Reactive Forms

app.module.ts
-------------
Add these into your app.module.ts file to use reactive forms
<!-- language: typescript -->

    import { NgModule } from '@angular/core';
    import { BrowserModule } from '@angular/platform-browser';
    import { FormsModule, ReactiveFormsModule } from '@angular/forms';
    import { AppComponent } from './app.component';

    @NgModule({
        imports: [
            BrowserModule,
            FormsModule,
            ReactiveFormsModule,
        ],
        declarations: [ AppComponent ]
        providers: [],
        bootstrap: [ AppComponent ]
    })
    export class AppModule {}

app.component.ts
----------------
<!-- language: typescript -->
    
    import { Component,OnInit } from '@angular/core';
    import template from './app.component.html';
    import { FormGroup,FormBuilder,Validators } from '@angular/forms';
    import { matchingPasswords } from './validators';
    
    @Component({
        selector: 'app',
        template
    })
    export class AppComponent implements OnInit {
        addForm: FormGroup;

        constructor(private formBuilder: FormBuilder) {
        }

        ngOnInit() {
            this.addForm = this.formBuilder.group({
                username: ['', Validators.required],
                email: ['', Validators.required],
                role: ['', Validators.required],
                password: ['', Validators.required],
                password2: ['', Validators.required]
            }, { validator: matchingPasswords('password', 'password2') });
        };
    
        addUser() {
            if (this.addForm.valid) {
                var adduser = {
                    username: this.addForm.controls['username'].value,
                    email: this.addForm.controls['email'].value,
                    password: this.addForm.controls['password'].value,
                    profile: {
                        role: this.addForm.controls['role'].value,
                        name: this.addForm.controls['username'].value,
                        email: this.addForm.controls['email'].value
                    }
                };
            
                console.log(adduser);// adduser var contains all our form values. store it where you want 
                this.addForm.reset();// this will reset our form values to null 
            }
        }
         
    }

app.component.html
------------------
<!-- language: lang-html -->

    <div>
        <form [formGroup]="addForm">
            <input
                type="text"
                placeholder="Enter username"
                formControlName="username" />

            <input
                type="text"
                placeholder="Enter Email Address"
                formControlName="email"/>

            <input
                type="password"
                placeholder="Enter Password"
                formControlName="password" />

            <input
                type="password"
                placeholder="Confirm Password"
                name="password2"
                formControlName="password2" />

            <div class='error' *ngIf="addForm.controls.password2.touched">
                <div
                    class="alert-danger errormessageadduser"
                    *ngIf="addForm.hasError('mismatchedPasswords')">
                        Passwords do not match
                </div>
            </div>
            <select name="Role" formControlName="role">
                <option value="admin" >Admin</option>
                <option value="Accounts">Accounts</option>
                <option value="guest">Guest</option>
            </select>
            <br/>
            <br/>
            <button type="submit" (click)="addUser()">
                <span>
                    <i class="fa fa-user-plus" aria-hidden="true"></i>
                </span>
                Add User
            </button>
        </form>
    </div>

validators.ts
-------------
<!-- language: typescript -->

    export function matchingPasswords(passwordKey: string, confirmPasswordKey: string) {
        return (group: ControlGroup): {
            [key: string]: any
        } => {
            let password = group.controls[passwordKey];
            let confirmPassword = group.controls[confirmPasswordKey];
    
            if (password.value !== confirmPassword.value) {
                return {
                    mismatchedPasswords: true
                };
            }
        }
    }


## Template Driven Forms
## Template - `signup.component.html`
<!-- language: lang-html -->

    <form #signUpForm="ngForm" (ngSubmit)="onSubmit()">

      <div class="title">
        Sign Up
      </div>

      <div class="input-field">
        <label for="username">username</label>
        <input
          type="text"
          pattern="\w{4,20}"
          name="username"
          required="required"
          [(ngModel)]="signUpRequest.username" />
      </div>

      <div class="input-field">
        <label for="email">email</label>
        <input
          type="email"
          pattern="^\S+@\S+$"
          name="email"
          required="required"
          [(ngModel)]="signUpRequest.email" />
      </div>

      <div class="input-field">
        <label for="password">password</label>
        <input
          type="password"
          pattern=".{6,30}"
          required="required"
          name="password"
          [(ngModel)]="signUpRequest.password" />
      </div>

      <div class="status">
        {{ status }}
      </div>

      <button [disabled]="!signUpForm.form.valid" type="submit">
        <span>Sign Up</span>
      </button>

    </form>


## Component - `signup.component.ts`
<!-- language: typescript -->
    
    import { Component } from '@angular/core';

    import { SignUpRequest } from './signup-request.model';

    @Component({
      selector: 'app-signup',
      templateUrl: './signup.component.html',
      styleUrls: ['./signup.component.css']
    })
    export class SignupComponent {

      status: string;
      signUpRequest: SignUpRequest;

      constructor() {
        this.signUpRequest = new SignUpRequest();
      }

      onSubmit(value, valid) {
        this.status = `User ${this.signUpRequest.username} has successfully signed up`;
      }

    }

## Model - `signup-request.model.ts`
<!-- language: typescript -->

    export class SignUpRequest {

      constructor(
        public username: string="",
        public email: string="",
        public password: string=""
      ) {}

    }

## App Module - `app.module.ts`
<!-- language: typescript -->

    import { BrowserModule } from '@angular/platform-browser';
    import { NgModule } from '@angular/core';
    import { FormsModule } from '@angular/forms';

    import { AppComponent } from './app.component';
    import { SignupComponent } from './signup/signup.component';

    @NgModule({
      declarations: [
        AppComponent,
        SignupComponent
      ],
      imports: [
        BrowserModule,
        FormsModule
      ],
      bootstrap: [AppComponent]
    })
    export class AppModule { }


## App Component - `app.component.html`
<!-- language: lang-html -->

    <app-signup></app-signup>

