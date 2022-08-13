---
title: "Routing"
slug: "routing"
draft: false
images: []
weight: 9974
type: docs
toc: true
---

## Basic Routing
Router enables navigation from one view to another based on user interactions with the application. 

Following are the steps in implementing basic routing in Angular - 

**NOTE**: Ensure you have this tag:
      
<!-- language: lang-html -->
  
    <base href='/'> 

as the first child under your head tag in your index.html file. This element states that your app folder is the application root. Angular would then know how to organize your links. 
 
1.  Check if you are pointing to the correct/latest routing dependencies in package.json (using the latest version of Angular) and that you already did an `npm install` - 

    <!-- language: lang-json -->
        "dependencies": {
            "@angular/router": "^4.2.5"
        }

2.  Define the route as per its interface definition:

    <!-- language: lang-ts -->

        interface Route {
          path?: string;
          pathMatch?: string;
          component?: Type<any>;
        }

3.  In a routing file (`routes/app.routing.ts`), import all the components which you need to configure for different routing paths. Empty path means that view is loaded by default. ":" in the path indicates dynamic parameter passed to the loaded component.

    <!-- language: lang-ts -->

        import { Routes, RouterModule } from '@angular/router';
        import { ModuleWithProviders } from '@angular/core';
        import { BarDetailComponent } from '../components/bar-detail.component';
        import { DashboardComponent } from '../components/dashboard.component';
        import { LoginComponent } from '../components/login.component';
        import { SignupComponent } from '../components/signup.component';
    
        export const APP_ROUTES: Routes = [
            { path: '', pathMatch: 'full', redirectTo: 'login' },
            { path: 'dashboard', component: DashboardComponent },
            { path: 'bars/:id', component: BarDetailComponent },
            { path: 'login', component: LoginComponent },
            { path: 'signup',   component: SignupComponent }
        ];
        export const APP_ROUTING: ModuleWithProviders = RouterModule.forRoot(APP_ROUTES);

4.  In your `app.module.ts`, place this under `@NgModule([])` under `imports`:

    <!-- language: lang-ts -->
        // Alternatively, just import 'APP_ROUTES
        import {APP_ROUTING} from '../routes/app.routing.ts';
        @NgModule([
            imports: [
                APP_ROUTING
                // Or RouterModule.forRoot(APP_ROUTES)
            ]
        ])

5.  Load/display the router components based on path accessed. The `<router-outlet> `directive is used to tell angular where to load the component.

    <!-- language: lang-ts -->

        import { Component } from '@angular/core';

        @Component({
            selector: 'demo-app',
            template: `
                <div>
                    <router-outlet></router-outlet>
                </div>
            `
        })
        export class AppComponent {}

5.  Link the other routes. By default, `RouterOutlet` will load the component for which empty path is specified in the `Routes`. `RouterLink` directive is used with html anchor tag to load the components attached to routes. `RouterLink` generates the href attribute which is used to generate links. For example:

    <!-- language: lang-ts -->

        import { Component } from '@angular/core';

        @Component({
            selector: 'demo-app',
            template: `
                <a [routerLink]="['/login']">Login</a>
                <a [routerLink]="['/signup']">Signup</a>      
                <a [routerLink]="['/dashboard']">Dashboard</a>
                <div>
                    <router-outlet></router-outlet>
                </div>
                `
        })
        export class AnotherComponent { }

Now, we are good with routing to static paths. `RouterLink` supports dynamic path too by passing extra parameters along with the path. 

<!-- language: lang-ts -->
    import { Component } from '@angular/core';

    @Component({
      selector: 'demo-app',
      template: `
            <ul>
              <li *ngFor="let bar of bars | async">
                <a [routerLink]="['/bars', bar.id]">
                  {{bar.name}}
                </a>
              </li>
            </ul>
        <div>
          <router-outlet></router-outlet>
        </div>
      `
    })
    export class SecondComponent { }

`RouterLink` takes an array where the first parameter is the path for routing and subsequent elements are for the dynamic routing parameters.


## Routing with children
I found this to be the way to properly nest children routes inside the app.routing.ts or app.module.ts file (depending on your preference). This approach works when using either WebPack or SystemJS.

The example below shows routes for home, home/counter, and home/counter/fetch-data.  The first and last routes being examples of redirects. Finally at the end of the example is a proper way to export the Route to be imported in a separate file. For ex. app.module.ts

To further explain, Angular requires that you have a pathless route in the children array that includes the parent component, to represent the parent route.  It's a little confusing but if you think about a blank URL for a child route, it would essentially equal the same URL as the parent route.

    import { NgModule } from "@angular/core";
    import { RouterModule, Routes } from "@angular/router";
    
    import { HomeComponent } from "./components/home/home.component";
    import { FetchDataComponent } from "./components/fetchdata/fetchdata.component";
    import { CounterComponent } from "./components/counter/counter.component";
    
    const appRoutes: Routes = [
        {
            path: "",
            redirectTo: "home",
            pathMatch: "full"
        },
        {
            path: "home",            
            children: [
                {
                    path: "",
                    component: HomeComponent
                },
                {
                    path: "counter",                    
                    children: [
                        {
                            path: "",
                            component: CounterComponent
                        },
                        {
                            path: "fetch-data",
                            component: FetchDataComponent                            
                        }
                    ]
                }
            ]
        },
        {
            path: "**",
            redirectTo: "home"
        }
    ];
    
    @NgModule({
        imports: [
            RouterModule.forRoot(appRoutes)
        ],
        exports: [
            RouterModule
        ]
    })
    export class AppRoutingModule { }

[Great Example and Description via Siraj][1]


  [1]: http://stackoverflow.com/a/38275995/1971015

