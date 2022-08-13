---
title: "Social Login with Angularfire2Firebase"
slug: "social-login-with-angularfire2firebase"
draft: false
images: []
weight: 9951
type: docs
toc: true
---

## Native Facebook Login with Angularfire2/Firebase
app.ts
<!-- language: typescript -->

    import {Component} from '@angular/core';
    import {Platform, ionicBootstrap} from 'ionic-angular';
    import {StatusBar} from 'ionic-native';
    import {LoginPage} from './pages/login/login';
    import {FIREBASE_PROVIDERS, defaultFirebase, AuthMethods, AuthProviders, firebaseAuthConfig} from 'angularfire2';
    
    @Component({
      template: '<ion-nav [root]="rootPage"></ion-nav>'
    })
    
    export class MyApp {
    
      private rootPage: any;
    
      constructor(private platform: Platform) {
        this.rootPage = LoginPage;
    
        platform.ready().then(() => {
          // Okay, so the platform is ready and our plugins are available.
          // Here you can do any higher level native things you might need.
          StatusBar.styleDefault();
        });
      }
    }
    
    ionicBootstrap(MyApp, [
      FIREBASE_PROVIDERS,
      defaultFirebase({
        apiKey: myAppKey,
        authDomain: 'myapp.firebaseapp.com',
        databaseURL: 'https://myapp.firebaseio.com',
        storageBucket: 'myapp.appspot.com',
      }),
      firebaseAuthConfig({})
    ]);

login.html

<!-- language: lang-html -->
    <ion-header>
      <ion-navbar>
        <ion-title>Home</ion-title>
      </ion-navbar>
    </ion-header>
    
    <ion-content padding class="login">
      <button (click)="facebookLogin()">Login With Facebook</button>
    </ion-content>

login.ts

<!-- language: typescript -->
    import {Component} from '@angular/core';
    import {Platform} from 'ionic-angular';
    import {AngularFire, AuthMethods, AuthProviders} from 'angularfire2';
    import {Facebook} from 'ionic-native';
    
    declare let firebase: any; // There is currently an error with the Firebase files, this will fix it.
    
    @Component({
      templateUrl: 'build/pages/login/login.html'
    })
    export class LoginPage {
    
      constructor(private platform: Platform, public af: AngularFire) {
    
      }
    
      facebookLogin() {
        Facebook.login(['public_profile', 'email', 'user_friends'])
          .then(success => {
            console.log('Facebook success: ' + JSON.stringify(success));
            let creds = firebase.auth.FacebookAuthProvider.credential(success.authResponse.accessToken);
            this.af.auth.login(creds, {
              provider: AuthProviders.Facebook,
              method: AuthMethods.OAuthToken,
              remember: 'default',
              scope: ['email']
            }).then(success => {
              console.log('Firebase success: ' + JSON.stringify(success));
            }).catch(error => {
              console.log('Firebase failure: ' + JSON.stringify(error));
            });
          }).catch(error => {
            console.log('Facebook failure: ' + JSON.stringify(error));
          });
      }
    }






