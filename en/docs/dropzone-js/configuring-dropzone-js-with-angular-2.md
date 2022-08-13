---
title: "Configuring dropzone.js with Angular 2"
slug: "configuring-dropzonejs-with-angular-2"
draft: false
images: []
weight: 9997
type: docs
toc: true
---

Basically Dropzone is lightweight librery for uploading files. t you can use to build your user own interface.The main feature of Dropzone is its events that you can listen to, so you can react to every change. you can easily Configuring with Angular 2


## Angular2  Dropzone-wrapper for anguler 2
This is an Angular 2 wrapper library for [Dropzone][1].


> npm install angular2-dropzone-wrapper --save-dev

**Load the module for your app**

    import { DropzoneModule } from 'angular2-dropzone-wrapper';
    import { DropzoneConfigInterface } from 'angular2-dropzone-wrapper';
     
    const DROPZONE_CONFIG: DropzoneConfigInterface = {
      // Change this to your upload POST address: 
      server: 'https://httpbin.org/post',
      maxFilesize: 50,
      acceptedFiles: 'image/*'
    };
     
    @NgModule({
      ...
      imports: [
        ...
        DropzoneModule.forRoot(DROPZONE_CONFIG)
      ]
    })

> COMPONENT USAGE
> 
> Simply replace the element that would oridinarily be passed to
> Dropzone with the dropzone component.

    <dropzone [config]="config" [message]="'Click or drag images here to upload'" (error)="onUploadError($event)" (success)="onUploadSuccess($event)"></dropzone>

**Create dropzone component**

    import {Component} from '@angular/core';
    @Component({
        selector: 'neon-new-media',
        templateUrl: './dropzone.component.html',
        styleUrls: ['./dropzone.component.scss']
    })
    export class DropZoneComponent {
    
     
        onUploadError(args: any) {
            console.log('onUploadError:', args);
        }
    
        onUploadSuccess(args: any) {
            console.log('onUploadSuccess:', args);
        }
    }




  [1]: http://www.dropzonejs.com/

