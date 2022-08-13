---
title: "Angular2 In Memory Web API"
slug: "angular2-in-memory-web-api"
draft: false
images: []
weight: 9792
type: docs
toc: true
---

I mainly requested this topic because I could not find any information on setting up multiple API routes with the Angular2-In-Memory-Web-Api. Ended up figuring it out myself, and figured this might be helpful to others.

## Setting Up Multiple Test API Routes
<strong>mock-data.ts</strong>

    export class MockData {
        createDb() {
            let mock = [
                { id: '1', name: 'Object A' },
                { id: '2', name: 'Object B' },
                { id: '3', name: 'Object C' }
            ];
            
            let data = [
                { id: '1', name: 'Data A' },
                { id: '2', name: 'Data B' },
                { id: '3', name: 'Data C' }
            ];
    
            return { mock, data };
        }
    }

Now, you can interact with both <pre>app/mock</pre> and <pre>app/data</pre> to extract their corresponding data.

## Basic Setup
<strong>mock-data.ts</strong>

Create the mock api data

    export class MockData {
      createDb() {
        let mock = [
          { id: '1', name: 'Object A' },
          { id: '2', name: 'Object B' },
          { id: '3', name: 'Object C' },
          { id: '4', name: 'Object D' }
        ];
        
        return {mock};
      }
    }

<strong>main.ts</strong>

Have the dependency injector provide the InMemoryBackendService for XHRBackend requests. Also, provide a class that includes a <pre>createDb()</pre> function (in this case, MockData) specifying the mocked API routes for SEED_DATA requests.

    import { XHRBackend, HTTP_PROVIDERS } from '@angular/http';
    import { InMemoryBackendService, SEED_DATA } from 'angular2-in-memory-web-api';
    import { MockData } from './mock-data';
    import { bootstrap } from '@angular/platform-browser-dynamic';
    
    import { AppComponent } from './app.component';
    
    bootstrap(AppComponent, [
        HTTP_PROVIDERS,
        { provide: XHRBackend, useClass: InMemoryBackendService },
        { provide: SEED_DATA,  useClass: MockData }
    ]);

<strong>mock.service.ts</strong>

Example of calling a get request for the created API route

    import { Injectable }     from '@angular/core';
    import { Http, Response } from '@angular/http';
    import { Mock } from './mock';
    
    @Injectable()
    export class MockService {
      // URL to web api
      private mockUrl = 'app/mock';
    
      constructor (private http: Http) {}
    
      getData(): Promise<Mock[]> {
        return this.http.get(this.mockUrl)
                        .toPromise()
                        .then(this.extractData)
                        .catch(this.handleError);
      }

      private extractData(res: Response) {
        let body = res.json();
        return body.data || { };
      }
    
      private handleError (error: any) {
        let errMsg = (error.message) ? error.message :
          error.status ? `${error.status} - ${error.statusText}` : 'Server error';
        console.error(errMsg);
        return Promise.reject(errMsg);
      }
    }

