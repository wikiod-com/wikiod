---
title: "Dropzone in Angular2"
slug: "dropzone-in-angular2"
draft: false
images: []
weight: 9941
type: docs
toc: true
---

## Dropzone
Angular 2 wrapper library for Dropzone.


> npm install angular2-dropzone-wrapper --save-dev

**Load the module for your app-module**

    import { DropzoneModule } from 'angular2-dropzone-wrapper';
    import { DropzoneConfigInterface } from 'angular2-dropzone-wrapper';
     
    const DROPZONE_CONFIG: DropzoneConfigInterface = {
      // Change this to your upload POST address: 
      server: 'https://example.com/post',
      maxFilesize: 10,
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
        selector: 'app-new-media',
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

