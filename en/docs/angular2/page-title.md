---
title: "Page title"
slug: "page-title"
draft: false
images: []
weight: 9991
type: docs
toc: true
---

How can you change the title of the page

## Syntax
 - `setTitle(newTitle: string): void;`
 - `getTitle(): string;`

## changing the page title
    

 1. First we need to provide Title service.
 2. Using setTitle


    import {Title} from "@angular/platform-browser"; 
    @Component({
      selector: 'app',
      templateUrl: './app.component.html',
      providers : [Title]
    })
    
    export class AppComponent implements {
       constructor( private title: Title) { 
         this.title.setTitle('page title changed');
       }
    }

