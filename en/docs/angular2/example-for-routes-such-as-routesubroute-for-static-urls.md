---
title: "Example for routes such as routesubroute for static urls"
slug: "example-for-routes-such-as-routesubroute-for-static-urls"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

## Basic route example with sub routes tree
app.module.ts
   
     import {routes} from "./app.routes";

     @NgModule({
        declarations: [AppComponent],
        imports: [BrowserModule, mainModule.forRoot(), RouterModule.forRoot(routes)],
        providers: [],
        bootstrap: [AppComponent]
     })

     export class AppModule { } 

app.routes.ts

    import { Routes } from '@angular/router';
    import {SubTreeRoutes} from "./subTree/subTreeRoutes.routes";

    export const routes: Routes = [
      ...SubTreeRoutes,
      { path: '',  redirectTo: 'home', pathMatch: 'full'}
    ];


subTreeRoutes.ts

    import {Route} from '@angular/router';
    import {exampleComponent} from "./example.component";
    
    export const SubTreeRoutes: Route[] = [
      {
        path: 'subTree',
        children: [
          {path: '',component: exampleComponent}
        ]
      }
    ];








