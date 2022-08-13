---
title: "Services and Dependency Injection"
slug: "services-and-dependency-injection"
draft: false
images: []
weight: 9937
type: docs
toc: true
---

## Example service

_services/my.service.ts_
```typescript
import { Injectable } from '@angular/core';

@Injectable()
export class MyService {
  data: any = [1, 2, 3];

  getData() {
    return this.data;
  }
}
```

 The service provider registration in the bootstrap method will make the service available globally.



_main.ts_
```typescript
import { bootstrap } from '@angular/platform-browser-dynamic';
import { AppComponent } from 'app.component.ts';
import { MyService } from 'services/my.service';

bootstrap(AppComponent, [MyService]);
```

In version RC5 global service provider registration can be done inside the module file.
In order to get a single instance of your service for your whole application the service should be declared in the providers list  in the ngmodule of your application.
_app_module.ts_

```typescript
import { NgModule }       from '@angular/core';
import { BrowserModule  } from '@angular/platform-browser';
import { routing, appRoutingProviders } from './app-routes/app.routes';
import { HttpModule} from '@angular/http';

import { AppComponent }   from './app.component';
import { MyService } from 'services/my.service';

import { routing } from './app-resources/app-routes/app.routes';

@NgModule({
    declarations: [ AppComponent ],
    imports:      [ BrowserModule,
                    routing,
                    RouterModule,
                    HttpModule ],
    providers: [    appRoutingProviders,
                    MyService
    ],
    bootstrap:    [AppComponent],
})
export class AppModule {}
```


Usage in `MyComponent`

_components/my.component.ts_

Alternative approach to register application providers in application components.
If we add providers at component level whenever the component is rendered it will create a new instance of the service.

```typescript
import { Component, OnInit } from '@angular/core';
import { MyService } from '../services/my.service';

@Component({
    ...
    ...
    providers:[MyService] //
 })
export class MyComponent implements OnInit {
    data: any[];
    // Creates private variable myService to use, of type MyService
    constructor(private myService: MyService) { }

    ngOnInit() {
        this.data = this.myService.getData();
    }
}
```

## Example with Promise.resolve
*services/my.service.ts*

    import { Injectable } from '@angular/core';

    @Injectable()
    export class MyService {
        data: any = [1, 2, 3];

        getData() {
            return Promise.resolve(this.data);
        }
    }

`getData()` now acts likes a REST call that creates a Promise, which gets resolved immediately. The results can be handheld inside `.then()` and errors can also be detected. This is good practice and convention for asynchronous methods.

*components/my.component.ts*

    import { Component, OnInit } from '@angular/core';
    import { MyService } from '../services/my.service';

    @Component({...})
    export class MyComponent implements OnInit {
        data: any[];
        // Creates private variable myService to use, of type MyService
        constructor(private myService: MyService) { }

        ngOnInit() {
            // Uses an "arrow" function to set data
            this.myService.getData().then(data => this.data = data);
        }
    }

## Testing a Service
Given a service that can login a user:

    import 'rxjs/add/operator/toPromise';
    
    import { Http } from '@angular/http';
    import { Injectable } from '@angular/core';
    
    interface LoginCredentials {
      password: string;
      user: string;
    }
    
    @Injectable()
    export class AuthService {
      constructor(private http: Http) { }
    
      async signIn({ user, password }: LoginCredentials) {
        const response = await this.http.post('/login', {
          password,
          user,
        }).toPromise();
    
        return response.json();
      }
    }

It can be tested like this:

    import { ConnectionBackend, Http, HttpModule, Response, ResponseOptions } from '@angular/http';
    import { TestBed, async, inject } from '@angular/core/testing';
    
    import { AuthService } from './auth.service';
    import { MockBackend } from '@angular/http/testing';
    import { MockConnection } from '@angular/http/testing';
    
    describe('AuthService', () => {
      beforeEach(() => {
        TestBed.configureTestingModule({
          imports: [HttpModule],
          providers: [
            AuthService,
            Http,
            { provide: ConnectionBackend, useClass: MockBackend },
          ]
        });
      });
    
      it('should be created', inject([AuthService], (service: AuthService) => {
        expect(service).toBeTruthy();
      }));
    
      // Alternative 1
      it('should login user if right credentials are passed', async(
        inject([AuthService], async (authService) => {
          const backend: MockBackend = TestBed.get(ConnectionBackend);
          const http: Http = TestBed.get(Http);
    
          backend.connections.subscribe((c: MockConnection) => {
            c.mockRespond(
              new Response(
                new ResponseOptions({
                  body: {
                    accessToken: 'abcdef',
                  },
                }),
              ),
            );
          });
    
          const result = await authService.signIn({ password: 'ok', user: 'bruno' });
    
          expect(result).toEqual({
            accessToken: 'abcdef',
          });
        }))
      );

      // Alternative 2
      it('should login user if right credentials are passed', async () => {
        const backend: MockBackend = TestBed.get(ConnectionBackend);
        const http: Http = TestBed.get(Http);
    
        backend.connections.subscribe((c: MockConnection) => {
          c.mockRespond(
            new Response(
              new ResponseOptions({
                body: {
                  accessToken: 'abcdef',
                },
              }),
            ),
          );
        });
    
        const authService: AuthService = TestBed.get(AuthService);
    
        const result = await authService.signIn({ password: 'ok', user: 'bruno' });
    
        expect(result).toEqual({
          accessToken: 'abcdef',
        });
      });

      // Alternative 3
      it('should login user if right credentials are passed', async (done) => {
        const authService: AuthService = TestBed.get(AuthService);
    
        const backend: MockBackend = TestBed.get(ConnectionBackend);
        const http: Http = TestBed.get(Http);
    
        backend.connections.subscribe((c: MockConnection) => {
          c.mockRespond(
            new Response(
              new ResponseOptions({
                body: {
                  accessToken: 'abcdef',
                },
              }),
            ),
          );
        });
    
        try {
          const result = await authService.signIn({ password: 'ok', user: 'bruno' });
    
          expect(result).toEqual({
            accessToken: 'abcdef',
          });
    
          done();
        } catch (err) {
          fail(err);
          done();
        }
      });
    });



