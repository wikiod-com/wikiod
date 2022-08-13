---
title: "md-icon"
slug: "md-icon"
draft: false
images: []
weight: 9997
type: docs
toc: true
---

## Creating an icon
The following is a guide on how to create an icon from material design icons:

1. Load the icon font from Google CDN in your `index.html`:
   <!-- language: lang-html -->
       <link href="https://fonts.googleapis.com/icon?family=Material+Icons" rel="stylesheet">
   Alternatively, you may import it in your `styles.css`:
   <!-- language: lang-css -->
       @import url('https://fonts.googleapis.com/icon?family=Material+Icons');
2. Use it as follows:
   <!-- language: lang-html -->
       <md-icon>menu</md-icon>

You're done!

## Using SVG Icons
This example shows how to use SVG icons in your app.

1. Download the SVG iconset / icon (in this case, we're getting the icons from <https://materialdesignicons.com/getting-started>.
2. Save it under your `assets` folder or somewhere else which you can access with.
3. In your `app.module.ts`, add the following code:
   <!-- language: lang-ts -->
       import { MdIconRegistry } from '@angular/material';
       import { DomSanitizer } from '@angular/platform-browser';

       export class AppModule {
           constructor(mdIconRegistry: MdIconRegistry, domSanitizer: DomSanitizer){
               // Note that you have to sanitize the resource since Angular will complain that it will cause XSS problems.
               // More info: https://g.co/ng/security#xss               
               mdIconRegistry.addSvgIconSet(domSanitizer.bypassSecurityTrustResourceUrl('assets/icons.svg'))
           }
       }
4. Use it via the `svgIcon` attribute:
   <!-- language: lang-html -->
       
       <md-icon svgIcon="menu"></md-icon>

