---
title: "Geolocation"
slug: "geolocation"
draft: false
images: []
weight: 9962
type: docs
toc: true
---

## Simple usage
In your `package.json` make sure to include the dependencies: 

    {
        ...
        "dependencies": {
            ...
            "ionic-native": "^1.3.10",
            ...
        },
        ...
    }
    
To use geolocation:

    // custom-component.ts

    import {Geolocation} from 'ionic-native';
    import template from './custom-component.html';
    
    @Component({
        selector: 'custom-component',
        template: template
    })
    export class CustomComponent {
    
        constructor() {

            // get the geolocation through a promise
            Geolocation.getCurrentPosition().then((position:Geoposition)=> {
                console.log(
                    position.coords.latitude,
                    position.coords.longitude);
            });
        }
    }

## Watching the position
For a more real time solution you can use watchPosition function in Geolocation that notifies whenever an error or a position change occurs.
Unlike the getCurrentPosition the watchPosition returns an Observable

    import {Geolocation} from 'ionic-native';
    import template from './custom-component.html';
    
    @Component({
    selector: 'custom-component',
    template: template
    })
    export class CustomComponent {
    constructor() {

        // get the geolocation through an observable
            Geolocation.watchPosition(<GeolocationOptions>{
                maximumAge: 5000, // a maximum age of cache is 5 seconds
                timeout: 10000, // time out after 10 seconds
                enableHighAccuracy: true // high accuracy 
            }).subscribe((position) => {
                console.log('Time:' + position.timestamp);
                console.log(
                    'Position:' + position.coords.latitude + ',' +
                    position.coords.longitude);
                console.log('Direction:' position.coords.heading);
                console.log('Speed:' position.coords.speed);

            });
    }




