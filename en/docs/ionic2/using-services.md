---
title: "Using Services"
slug: "using-services"
draft: false
images: []
weight: 9898
type: docs
toc: true
---

One very important thing about using shared services, is that they must be included in the `providers` array of the top-most component where they must be shared.

Why is that? Well, Let's suppose that we include the `MyService` reference in the `providers` array from each `Component`. Something like:

    @Component({
      templateUrl:"page1.html",
      providers: [MyService]
    })

And 

    @Component({
      templateUrl:"page2.html",
      providers: [MyService]
    })

That way **a new instance of the service will be created for each component** so the instance where one page will save the data, will be different from the instance used to get the data. So that won't work.

In order to make the entire app use the same instance (making the service work as a *singleton* service) we can add its reference in the `App Component` like this:

    @Component({
      template: '<ion-nav [root]="rootPage"></ion-nav>',
      providers: [MyService]
    })

You could also add the `MyService` reference in the `ionicBootstrap(MyApp, [MyService]);` but according [Angular2 style guides](https://angular.io/styleguide#!#07-03)

> Do provide services to the Angular 2 injector at the top-most
> component where they will be shared.
> 
> Why? The Angular 2 injector is hierarchical.
> 
> Why? When providing the service to a top level component, that
> instance is shared and available to all child components of that top
> level component.
> 
> Why? This is ideal when a service is sharing methods or state.
> 
> Why? This is not ideal when two different components need different
> instances of a service. In this scenario it would be better to provide
> the service at the component level that needs the new and separate
> instance.

And

> It will work. It's just not a best practice. **The bootstrap provider
> option is intended for configuring and overriding Angular's own
> preregistered services**, such as its routing support.

... the `App Component` would be the best choice.

## Share information between different pages
One of the easiest examples of using *shared services* is when we want to store some data from a given page of our application, and then get that data again but from another page.

One option could be to send that data as a parameter (for instance, if one page calls the other one) but if we want to use that data from a completely different part of the application, that seems to be not the best way to do it. That's when _shared services_ comes to play.

In this example, we're going to use a simple service called `MyService` which only has two simple methods: `saveMessage()` to store a string and `getMessage()` to get it again. This code is part of [this working plunker](http://plnkr.co/edit/EOFTax?p=preview) where you can see it in action.

    import {Injectable} from '@angular/core';
    
    @Injectable()
    export class MyService { 
      
      private message: string;
      
      constructor(){ }
      
      public saveMessage(theMessage: string): void {
        this.message = theMessage;
      }
      
      public getMessage(): string {
        return this.message;
      }
    }

Then, when we want to store a new message, we can just use the `saveMessage(theMessageWeWantToSave);` method from the `MyService` instance (called just `service`).

    import { Component } from "@angular/core";
    import { MyService } from 'service.ts';
    
    @Component({
      templateUrl:"page1.html"
    })
    export class Page1 {
      
      message: string;
      
      // ... 
       
      public saveSecretMessage(): void {
        this.service.saveMessage(this.message);
      }      
    }

In the same way, when we want to get that data we can use the `getMessage()` method from the service instance like this:

    import { Component } from "@angular/core";
    import { MyService } from 'service.ts';
    
    @Component({
      templateUrl:"page2.html"
    })
    export class Page2 {
      
      enteredMessage: string;
      
      constructor(private service: MyService) {
        this.enteredMessage = this.service.getMessage();
      }
        
      // ...
    }

Please remember to check the *Remarks* section to see where should the reference for the `MyService` service be included and why.

