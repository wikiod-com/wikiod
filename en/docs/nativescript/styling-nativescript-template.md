---
title: "Styling nativescript template"
slug: "styling-nativescript-template"
draft: false
images: []
weight: 9973
type: docs
toc: true
---

## Adding a sample layout in your app
*main.component.ts*

    import {Component} from "@angular/core";
    
    @Component({
        selector: "main",
        template: `
        <StackLayout>
          <TextField hint="some text"></TextField>
          <Button text="Click me" class="btn"></Button>
        </StackLayout>
        `,
        styleUrls: ["pages/main/main-common.css", "pages/main/main.css"]
    })
    export class MainComponent { }

# Method 1 : Global CSS

*app.css* -- Applies globally to all layouts.

    StackLayout {
      margin: 10;
      background-color: white;
    }
    .btn, TextField {
      margin-left: 16;
      margin-right: 16;
    }

# Method 2 : Platform specific CSS

*platform.android.css* -- Applies globally to all layouts in android device.

    .btn{
        background-color: #191919;
        color: #fff;
    }

*platform.ios.css* -- Applies globally to all layouts in ios device.

    .btn{
        background-color: #fff;
        color: #191919;
    }

*app.css*

    @import url("~/platform.css");

# Method 3 : Component-specific CSS

*pages/main/main.android.css* -- Applies to specific component in android device.

    TextField {
      color: #e1e1e1;
      font-size: 12;
    }

*pages/main/main.ios.css* -- Applies to specific component in ios device.

    TextField {
      color: #e3e3e3;
      font-size: 15;
    }

*pages/main/main-common.css*  -- Applies to specific component in all devices.

    TextField {
      padding: 4;
    }





