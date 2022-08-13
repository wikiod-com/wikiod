---
title: "Advanced Component Examples"
slug: "advanced-component-examples"
draft: false
images: []
weight: 9898
type: docs
toc: true
---

Remember that Angular 2 is all about singular responsibility. No matter how small your component is, dedicate a separate logic for each and every component. Be it a button, a fancy anchor link, a dialog header or even a sidenav's sub item.

## Image Picker with Preview
In this example, we are going to create an image picker that previews your picture before uploading. The previewer also supports drag and dropping files into the input. In this example, I am only going to cover uploading single files, but you can tinker a bit to get multi file upload working.

*image-preview.html*

This is the html layout of our image preview
```html
<!-- Icon as placeholder when no file picked -->
<i class="material-icons">cloud_upload</i>

<!-- file input, accepts images only. Detect when file has been picked/changed with Angular's native (change) event listener -->
<input type="file" accept="image/*" (change)="updateSource($event)">

<!-- img placeholder when a file has been picked. shows only when 'source' is not empty -->
<img *ngIf="source" [src]="source" src="">
```

*image-preview.ts*

This is the main file for our `<image-preview>` component
```TypeScript
import {
    Component,
    Output,
    EventEmitter,
} from '@angular/core';

@Component({
    selector: 'image-preview',
    styleUrls: [ './image-preview.css' ],
    templateUrl: './image-preview.html'
})
export class MtImagePreviewComponent {

    // Emit an event when a file has been picked. Here we return the file itself
    @Output() onChange: EventEmitter<File> = new EventEmitter<File>();

    constructor() {}

    // If the input has changed(file picked) we project the file into the img previewer
    updateSource($event: Event) {
        // We access he file with $event.target['files'][0]
        this.projectImage($event.target['files'][0]);
    }

    // Uses FileReader to read the file from the input
    source:string = '';
    projectImage(file: File) {
        let reader = new FileReader;
        // TODO: Define type of 'e'
        reader.onload = (e: any) => {
            // Simply set e.target.result as our <img> src in the layout
            this.source = e.target.result;
            this.onChange.emit(file);
        };
        // This will process our file and get it's attributes/data
        reader.readAsDataURL(file);
    }
}
```

*another.component.html*
```html
<form (ngSubmit)="submitPhoto()">
    <image-preview (onChange)="getFile($event)"></image-preview>
    <button type="submit">Upload</button>
</form>
```

And that's it. Way more easier than it was in AngularJS 1.x. I actually made this component based on an older version I made in AngularJS 1.5.5.

## Filter out table values by the input
Import `ReactiveFormsModule`, and then

    import { Component, OnInit, OnDestroy } from '@angular/core';
    import { FormControl } from '@angular/forms';
    import { Subscription } from 'rxjs';
    
    @Component({
      selector: 'component',
      template: `
        <input [formControl]="control" />
        <div *ngFor="let item of content">
          {{item.id}} - {{item.name}}
        </div>
      `
    })
    export class MyComponent implements OnInit, OnDestroy {
    
      public control = new FormControl('');
    
      public content: { id: number; name: string; }[];
      
      private originalContent = [
        { id: 1, name: 'abc' },
        { id: 2, name: 'abce' },
        { id: 3, name: 'ced' }
      ];
      
      private subscription: Subscription;
      
      public ngOnInit() {
        this.subscription = this.control.valueChanges.subscribe(value => {
          this.content = this.originalContent.filter(item => item.name.startsWith(value));
        });
      }
      
      public ngOnDestroy() {
        this.subscription.unsubscribe();
      }
      
    }



