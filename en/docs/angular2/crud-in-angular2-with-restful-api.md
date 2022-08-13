---
title: "CRUD in Angular2 with Restful API"
slug: "crud-in-angular2-with-restful-api"
draft: false
images: []
weight: 9783
type: docs
toc: true
---

## Syntax
- @Injectable() // Tells dependency injector to inject dependencies when creating instance of this service. 

- request.subscribe() // This is where you *actually* make the request. Without this your request won't be sent. Also you want to read response in the callback function. 
- constructor(private wikiService: WikipediaService) { } // Since both our service and it's dependencies are injectable by the dependency injector it's a good practice to inject the service to component for unit testing the app.

## Read from an Restful API in Angular2
To separate API logic from the component, we are creating the API client as a separate class. This example class makes a request to Wikipedia API to get random wiki articles.     

        import { Http, Response } from '@angular/http';
        import { Injectable } from '@angular/core';
        import { Observable }     from 'rxjs/Observable';
        import 'rxjs/Rx';
        
        @Injectable()
        export class WikipediaService{
            constructor(private http: Http) {}
        
            getRandomArticles(numberOfArticles: number)
            {
                var request = this.http.get("https://en.wikipedia.org/w/api.php?action=query&list=random&format=json&rnlimit=" + numberOfArticles);
                return request.map((response: Response) => {
                    return response.json();
                },(error) => {
                    console.log(error);
                    //your want to implement your own error handling here.
                });
            }
        }

And have a component to consume our new API client. 

    import { Component, OnInit } from '@angular/core';
    import { WikipediaService } from './wikipedia.Service';
    
    @Component({
        selector: 'wikipedia',
        templateUrl: 'wikipedia.component.html'
    })
    export class WikipediaComponent implements OnInit {
        constructor(private wikiService: WikipediaService) { }
    
        private articles: any[] = null;
        ngOnInit() { 
            var request = this.wikiService.getRandomArticles(5);
            request.subscribe((res) => {
                this.articles = res.query.random;
            });
        }
    }

