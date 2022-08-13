---
title: "Angular2 CanActivate"
slug: "angular2-canactivate"
draft: false
images: []
weight: 9984
type: docs
toc: true
---

## Angular2 CanActivate
**Implemented in a router:**

    export const MainRoutes: Route[] = [{
       path: '',
       children: [ {
          path: 'main',
          component: MainComponent ,
          canActivate : [CanActivateRoute]
       }]
    }];

 **The `canActivateRoute` file:**

    @Injectable()
      export class  CanActivateRoute implements CanActivate{
      constructor(){}
      canActivate(next: ActivatedRouteSnapshot, state: RouterStateSnapshot): boolean {
         return true;
      }
    }

