---
title: "Implementing Animations in Nativescript"
slug: "implementing-animations-in-nativescript"
draft: false
images: []
weight: 9951
type: docs
toc: true
---

## Use of animation timing function and animation properties.
*pages/main.component.ts*

    import {Component, ElementRef, ViewChild} from "@angular/core";
    import {View} from "ui/core/view";
    import {AnimationCurve} from "ui/enums";
    
    @Component({
        selector: "main",
        template: `
            <StackLayout>
              <Image #img src="~/assets/images/user-shape.png"></Image>
              <Button text="Apply Changes" (tap)="animateImage()"></Button>
            </StackLayout>
        `,
        styleUrls: ["pages/main/main-common.css"],
    })
    
    export class MainComponent {
        @ViewChild("img") img: ElementRef;
        animateImage() {
            let img = <View>this.img.nativeElement;
            img.animate({
                translate: { x: 0, y: 120 },
                duration: 2000,
                curve: AnimationCurve.easeIn
            });
        }
    }


**#snippet for other animation properties**

*You can also write your own timing function using cubicBezier.*

 1. Use of cubicBezier
   

    img.animate({
            translate: { x: 0, y: 120 },
            duration: 2000,
            curve: AnimationCurve.cubicBezier(0.1, 0.2, 0.1, 1)
      });

 2. Animation Properties

 # Opacity

    img.animate({
        opacity: 0,
        duration: 2000
    });

# Translate

    img.animate({
        translate: { x: 120, y: 0},
        duration: 2000
    });

# Scale

    img.animate({
        scale: { x: 1.5, y: 1.5},
        duration: 2000
    });

# Rotate

    img.animate({
        rotate: 270,
        duration: 2000
    });
    



## Background Animation of StackLayout
Animating Background color of stacklayout on tapping button

*pages/main.component.ts*

    import {Component, ElementRef, ViewChild} from "@angular/core";
    import {Color} from "color";
    import {View} from "ui/core/view";
    
        @Component({
            selector: "main",
            template: `
                <StackLayout #el>
                  <Button text="Apply Changes" (tap)="changeBgColor()"></Button>
                </StackLayout>
            `,
            styleUrls: ["pages/main/main-common.css"],
        })
        
        export class MainComponent {
            @ViewChild("el") el: ElementRef;
            changeBgColor() {
                let el = <View>this.el.nativeElement;
                el.animate({
                    backgroundColor: new Color("#222"),
                    duration: 300
                });
            }
        }
        

*pages/main-common.css*

    StackLayout{
        background-color: #333;
    }

