---
title: "Routing"
slug: "routing"
draft: false
images: []
weight: 9769
type: docs
toc: true
---

## ResolveData
This example will show you how you can resolve data fetched from a service before rendering your application's view.

**Uses angular/router 3.0.0-beta.2 at the time of writing**

*users.service.ts*
```javascript
...
import { Http, Response } from '@angular/http';
import { Observable } from 'rxjs/Rx';
import { User } from './user.ts';



@Injectable()
export class UsersService {

    constructor(public http:Http) {}

    /**
     * Returns all users
     * @returns {Observable<User[]>}
     */
    index():Observable<User[]> {

        return this.http.get('http://mywebsite.com/api/v1/users')
            .map((res:Response) => res.json());
    }

    /**
     * Returns a user by ID
     * @param id
     * @returns {Observable<User>}
     */
    get(id:number|string):Observable<User> {

        return this.http.get('http://mywebsite.com/api/v1/users/' + id)
            .map((res:Response) => res.json());
    }
}
```

*users.resolver.ts*
```javascript
...
import { UsersService } from './users.service.ts';
import { Observable } from 'rxjs/Rx';
import {
    Resolve,
    ActivatedRouteSnapshot,
    RouterStateSnapshot
} from "@angular/router";




@Injectable()
export class UsersResolver implements Resolve<User[] | User> {

    // Inject UsersService into the resolver
    constructor(private service:UsersService) {}

    resolve(route:ActivatedRouteSnapshot, state:RouterStateSnapshot):Observable<User[] | User> {
        // If userId param exists in current URL, return a single user, else return all users
        // Uses brackets notation to access `id` to suppress editor warning, may use dot notation if you create an interface extending ActivatedRoute with an optional id? attribute
        if (route.params['id']) return this.service.get(route.params['id']);
        return this.service.index();
    }
}
```

*users.component.ts*

This is a page component with a list of all users. It will work similarly for User detail page component, replace `data.users` with `data.user` or whatever key defined in *app.routes.ts*(see below)
```javascript
...
import { ActivatedRoute} from "@angular/router";



@Component(...)
export class UsersComponent {

    users:User[];


    constructor(route: ActivatedRoute) {
        route.data.subscribe(data => {
            // data['Match key defined in RouterConfig, see below']
            this.users = data.users;
        });
    }

    /**
     * It is not required to unsubscribe from the resolver as Angular's HTTP
     * automatically completes the subscription when data is received from the server
     */
}
```

*app.routes.ts*
```javascript
...
import { UsersResolver } from './resolvers/users.resolver';



export const routes:RouterConfig = <RouterConfig>[
    ...
    {
        path: 'user/:id',
        component: UserComponent,
        resolve: {
            // hence data.user in UserComponent
            user: UsersResolver
        }
    },
    {
        path: 'users',
        component: UsersComponent,
        resolve: {
            // hence data.users in UsersComponent, note the pluralisation
            users: UsersResolver
        }
    },
    ...
]
...
```

*app.resolver.ts*

Optionally bundle multiple resolvers together.

**IMPORTANT:** *Services used in resolver must be imported first or you will get a 'No provider for ..Resolver error'. Remember that these services will be available globally and you will not need to declare them in any component's `providers` anymore. Be sure to unsubscribe from any subscription to prevent memory leak*
```javascript
...
import { UsersService } from './users.service';
import { UsersResolver } from './users.resolver';

export const ROUTE_RESOLVERS = [
    ...,
    UsersService,
    UsersResolver
]
```

*main.browser.ts*

Resolvers have to be injected during bootstrapping.
```javascript
...
import {bootstrap} from '@angular/platform-browser-dynamic';
import { ROUTE_RESOLVERS } from './app.resolver';

bootstrap(<Type>App, [
    ...
    ...ROUTE_RESOLVERS
])
.catch(err => console.error(err));
```





## Routing with Children
Contrary to original documentation, I found this to be the way to properly nest children routes inside the app.routing.ts or app.module.ts file (depending on your preference). This approach works when using either WebPack or SystemJS.

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

## Basic Routing
Router enables navigation from one view to another based on user interactions with the application. 

Following are the steps in implementing basic routing in Angular 2 - 

**Basic precaution**: Ensure you have the tag 
        
        <base href='/'> 

as the first child under your head tag in your index.html file. This tag tells that your app folder is the application root. Angular 2 would then know to organize your links. 
 
**First step** is to check if you are pointing to correct/latest routing dependencies in package.json - 

    "dependencies": {
      ......
      "@angular/router": "3.0.0-beta.1",
      ......
    }

**Second step** is to define the route as per it's class definition - 

    class Route {
      path : string
      pathMatch : 'full'|'prefix'
      component : Type|string
      .........
    }

In a routes file (`route/routes.ts`), import all the components which you need to configure for different routing paths. Empty path means that view is loaded by default. ":" in the path indicates dynamic parameter passed to the loaded component. 

Routes are made available to application via dependency injection. ProviderRouter method is called with RouterConfig as parameter so that it can be injected to the components for calling routing specific tasks.

    import { provideRouter, RouterConfig } from '@angular/router';
    import { BarDetailComponent } from '../components/bar-detail.component';
    import { DashboardComponent } from '../components/dashboard.component';
    import { LoginComponent } from '../components/login.component';
    import { SignupComponent } from '../components/signup.component';
    
    export const appRoutes: RouterConfig = [
      { path: '', pathMatch: 'full', redirectTo: 'login' },
      { path: 'dashboard', component: DashboardComponent },
      { path: 'bars/:id', component: BarDetailComponent },
      { path: 'login', component: LoginComponent },
      { path: 'signup',   component: SignupComponent }
    ];
    
    export const APP_ROUTER_PROVIDER = [provideRouter(appRoutes)];

**Third step** is to bootstrap the route provider.

In your `main.ts` (It can be any name. basically, it should your main file defined in systemjs.config)

    import { bootstrap } from '@angular/platform-browser-dynamic';
    import { AppComponent } from './components/app.component';
    import { APP_ROUTER_PROVIDER } from "./routes/routes";
    
    bootstrap(AppComponent, [ APP_ROUTER_PROVIDER ]).catch(err => console.error(err));

Fourth step is to load/display the router components based on path accessed. <router-outlet> directive is used to tell angular where to load the component. To use <router-outlet> import the ROUTER_DIRECTIVES. 

    import { ROUTER_DIRECTIVES } from '@angular/router';

    @Component({
      selector: 'demo-app',
      template: `
        ....................................
        <div>
          <router-outlet></router-outlet>
        </div>
        ....................................
      `,
      // Add our router directives we will be using
      directives: [ROUTER_DIRECTIVES]
    })

Fifth step is to link the other routes. By default, RouterOutlet will load the component for which empty path is specified in the RouterConfig. RouterLink directive is used with html anchor tag to load the components attached to routes. RouterLink generates the href attribute which is used to generate links. For Ex:

    import { Component } from '@angular/core';
    import { ROUTER_DIRECTIVES } from '@angular/router';

    @Component({
      selector: 'demo-app',
      template: `
        <a [routerLink]="['/login']">Login</a>
        <a [routerLink]="['/signup']">Signup</a>      
        <a [routerLink]="['/dashboard']">Dashboard</a>
        <div>
          <router-outlet></router-outlet>
        </div>
      `,
      // Add our router directives we will be using
      directives: [ROUTER_DIRECTIVES]
    })
    export class AppComponent { }

Now, we are good with routing to static path. RouterLink support dynamic path also by passing extra parameters along with the path. 

import { Component } from '@angular/core';
import { ROUTER_DIRECTIVES } from '@angular/router';

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
      `,
      // Add our router directives we will be using
      directives: [ROUTER_DIRECTIVES]
    })
    export class AppComponent { }

RouterLink takes an array where first element is the path for routing and subsequent elements are for the dynamic routing parameters.


## Child Routes
Sometimes it makes sense to nest view's or routes within one another. For example on the dashboard you want several sub views, similar to tabs but implemented via the routing system, to show the users' projects, contacts, messages ets. In order to support such scenarios the router allows us to define child routes.

First we adjust our `RouterConfig` from above and add the child routes:

    import { ProjectsComponent } from '../components/projects.component';
    import { MessagesComponent} from '../components/messages.component';

    export const appRoutes: RouterConfig = [
      { path: '', pathMatch: 'full', redirectTo: 'login' },
      { path: 'dashboard', component: DashboardComponent,
        children: [
          { path: '', redirectTo: 'projects', pathMatch: 'full' },
          { path: 'projects', component: 'ProjectsComponent' },
          { path: 'messages', component: 'MessagesComponent' }
        ] },
      { path: 'bars/:id', component: BarDetailComponent },
      { path: 'login', component: LoginComponent },
      { path: 'signup',   component: SignupComponent }
    ];

Now that we have our child routes defined we have to make sure those child routes can be displayed within our `DashboardComponent`, since that's where we have added the childs to. Previously we have learned that the components are displayed in a `<router-outlet></router-outlet>` tag. Similar we declare another `RouterOutlet` in the `DashboardComponent`:

    import { Component } from '@angular/core';
    
    @Component({
      selector: 'dashboard',
      template: `
        <a [routerLink]="['projects']">Projects</a>
        <a [routerLink]="['messages']">Messages</a>   
        <div>
          <router-outlet></router-outlet>
        </div>
      `
    })
    export class DashboardComponent { }

As you can see, we have added another `RouterOutlet` in which the child routes will be displayed. Usually the route with an empty path will be shown, however, we set up a redirect to the `projects` route, because we want that to be shown immediately when the `dashboard` route is loaded. That being said, we need an empty route, otherwise you'll get an error like this:

    Cannot match any routes: 'dashboard'

So by adding the *empty* route, meaning a route with an empty path, we have defined an entry point for the router.

