---
title: "Getting started with openlayers-3"
slug: "getting-started-with-openlayers-3"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Getting started with a simple map
<!-- language: lang-html -->

    <html>
      <head>
        <title>Getting started</title>
        <link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/ol3/3.17.1/ol.css" type="text/css">
        <script src="https://cdnjs.cloudflare.com/ajax/libs/ol3/3.17.1/ol.js"></script>
      </head>
      <body>
        <div id="map" class="map"></div>
        <script>
          var baseLayer= new ol.layer.Tile({ //a Tile layer is a the background layer for the map
            // here we choose an OpenStreetMap base layer
            source: new ol.source.OSM({
              url: 'https://a.tile.openstreetmap.org/{z}/{x}/{y}.png'
            }) 
          });
    
          var map = new ol.Map({ // we create our map
            layers: [baseLayer], // and add the layers to it ( in our case we only have one)
            target: 'map', // the div element that will serve as a map
            controls: ol.control.defaults({ // we leave the map controls to default
              attributionOptions: /** @type {olx.control.AttributionOptions} */ ({
                collapsible: false
              })
            }),
            view: new ol.View({ // we define the initial view of the map
              center: ol.proj.fromLonLat([0, 0]), //the default projection is the spherical mercator (meter units) so we get coordinates of the center by degrees
              zoom: 2 // the initial zoom level
            })
          });
        </script>
      </body>
    </html>

## Example using Bing Maps
<!-- language: lang-js -->
    var baseLayer = new ol.layer.Tile({
        visible: true,
        preload: Infinity,
        source: new ol.source.BingMaps({
            // We need a key to get the layer from the provider. 
            // Sign in with Bing Maps and you will get your key (for free)
            key: 'Ap9VqFbJYRNkatdxt3KyzfJxXN_9GlfABRyX3k_JsQTkMQLfK_-AzDyJHI5nojyP',
            imagerySet: 'Aerial', // or 'Road', 'AerialWithLabels', etc.
            // use maxZoom 19 to see stretched tiles instead of the Bing Maps
            // "no photos at this zoom level" tiles
            maxZoom: 19
        })
    });
    
    var map = new ol.Map({ 
        layers: [baseLayer],
        target: 'map', 
        controls: ol.control.defaults({ 
            attributionOptions: /** @type {olx.control.AttributionOptions} */ ({
                collapsible: false
            })
        }),
        view: new ol.View({ 
            center: ol.proj.fromLonLat([0, 0]),
            zoom: 2 
        })
    });
       

## Installation or Setup
OpenLayers 3 or as it is referred OL-3 is a Javascript Library for web mapping, so in order to use it you'll need to add it in your html:

- first add the ol.css file to use the map styling of OL-3 :

- then add the ol.js file :


you can also download OL-3 from the official site www.openlayers.org and call the files in the html by changing the src and href



## setting up OL-3
    <link rel="stylesheet" href="http://openlayers.org/en/v3.17.1/css/ol.css" type="text/css">

    <script src="http://openlayers.org/en/v3.17.1/build/ol.js"></script>

