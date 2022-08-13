---
title: "Google Maps JavaScript API v3 - Advanced"
slug: "google-maps-javascript-api-v3---advanced"
draft: false
images: []
weight: 9964
type: docs
toc: true
---

## Official Google Documentation

- [Google Maps JavaScript API Overview](https://developers.google.com/maps/documentation/javascript/)
- [Google Maps JavaScript API Code Samples](https://developers.google.com/maps/documentation/javascript/examples/)
- [Google Maps JavaScript API Reference](https://developers.google.com/maps/documentation/javascript/3.exp/reference)

## About the examples in this topic

- ***`YOUR_API_KEY`*** needs to be replaced by your own application API key. You can obtain an API key and configure it in the [Google API Console](https://console.developers.google.com/).

## Custom Styled Map
<!-- begin snippet: html hide: false console: true -->

<!-- language: lang-html -->

    <!DOCTYPE html>
    <html>
      <head>
        <title>Styled Maps</title>
        <meta charset="utf-8">
        <style>
          #map {
            height: 100%;
          }
        </style>
      </head>
      <body>
        <div id="map"></div>
        <script type="text/javascript">
            function initialize() {
            
                // Create an array of styles.
                var styles = [{
                    stylers: [{
                        hue: "#4679BD"
                    }, {
                        saturation: 100
                    }]
                }, {
                    featureType: "poi",
                    elementType: "labels",
                    stylers: [{
                        visibility: "off"
                    }]
                }, {
                    featureType: "administrative",
                    elementType: "labels",
                    stylers: [{
                        color: "#"
                    }]
                }, {
                    featureType: "road.local",
                    elementType: "geometry",
                    stylers: [{
                        visibility: "off"
                    }]
                }, {
                    featureType: "road",
                    elementType: "labels",
                    stylers: [{
                        visibility: "off"
                    }]
                }, {
                    featureType: "land",
                    elementType: "geometry",
                    stylers: [{
                        hue: "#e4cc55",
                        saturation: 100
                    }]
                }, {
                    featureType: "water",
                    elementType: "geometry",
                    stylers: [{
                        color: "#C5E7FF"
                    }]
                }, {
                    featureType: "transit.station.airport",
                    elementType: "geometry",
                    stylers: [{
                        hue: "#FF00CA"
                    }]
                }];
            
                // Create a new StyledMapType object, passing it the array of styles, as well as the name to be displayed on the map type control.
                var styledMap = new google.maps.StyledMapType(styles, {
                    name: "Styled Map"
                });
            
                // Create a map object, and include the MapTypeId(s) to add to the map type control.
                var mapOptions = {
                    zoom: 6,
                    center: new google.maps.LatLng(46.13, 6.14),
                    mapTypeControlOptions: {
                        mapTypeIds: [google.maps.MapTypeId.TERRAIN, 'custom_map_style']
                    }
                };
            
                // Create the map.
                var map = new google.maps.Map(document.getElementById('map-canvas'),
                    mapOptions);
            
                // Associate the styled map with the MapTypeId and set it to display.
                map.mapTypes.set('custom_map_style', styledMap);
                map.setMapTypeId('custom_map_style');
            }
        </script>
        <script src="https://maps.googleapis.com/maps/api/js?key=YOUR_API_KEY&callback=initialize"
        async defer></script>
      </body>
    </html>

<!-- end snippet -->

To create your own map style, please refer to the [Style Reference](https://developers.google.com/maps/documentation/javascript/style-reference) documentation and/or use the great [Styled Maps Wizard](https://googlemaps.github.io/js-samples/styledmaps/wizard/) tool.

<kbd>[JSFiddle demo](http://jsfiddle.net/upsidown/Lt2Ey/)</kbd>

