---
title: "Getting started with leaflet"
slug: "getting-started-with-leaflet"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Implementing Leaflet.js with HTML and JavaScript
To use Leaflet, load its stylesheet and JavaScript file to your page:

<!-- language: lang-html -->

    <link rel="stylesheet" href="/css/leaflet.css" />
    <script src="/js/leaflet.js"></script>

These resources can be downloaded from a variety of locations such as [Leaflet's homepage][1] or the [Leaflet Github repository][2], or you can directly use CDN as,

<!-- language: lang-html -->
    <link rel="stylesheet" href="https://unpkg.com/leaflet@1.0.3/dist/leaflet.css" />
    <script src="https://unpkg.com/leaflet@1.0.3/dist/leaflet.js" ></script>

You need a container for your map. It is common for developers to use a `div` with an id of "map" for this. Make sure to give your map `div` a height as well or the map might not show up.

<!-- language: lang-html -->

    <div id="map" style="height: 200px;"></div>

Initializing the map is done by creating a map object using the `Leaflet.map(mapContainerId)` method. In the below example, a latitude and longitude are set as a default with a default zoom level of 13.

<!-- language: lang-js -->

    var map = L.map('map').setView([42.35, -71.08], 13);

This creates our empty map, we should now provide a tile layer to act as our base map. A tilelayer is a service that provides map images in tiles, small images that are accessed by x, y and z parameters in a particular order (see below). 

A tile layer URL might look like this, where `{s}`, `{z}`, `{x}` and `{y}` are placeholders that Leaflet will automatically change during operation:

<!-- language: lang-js -->

    "http://{s}.domain.com/foo/{z}/{x}/{y}.png"

We can now add our tilelayer, along with attribution info and maximum possible zoom level, and add it to the map:

<!-- language: lang-js -->

    L.tileLayer('http://tiles.mapc.org/basemap/{z}/{x}/{y}.png',
    {
      attribution: 'Tiles by <a href="http://mapc.org">MAPC</a>, Data by <a href="http://mass.gov/mgis">MassGIS</a>',
      maxZoom: 17,
      minZoom: 5
    }).addTo(map);

**Note:** Map initialization and loading the tile layer need to occur after the inclusion of `leaflet.js` and the map container `div` element.


  [1]: http://www.leafletjs.com
  [2]: https://github.com/Leaflet/Leaflet

## Leaflet Quick Start
<!-- language: lang-html -->

    <!DOCTYPE html>
    <html>
    
      <head>
        <title>My Leaflet Map</title>
        <link rel="stylesheet" href="//unpkg.com/leaflet@1.0.2/dist/leaflet.css" />
        <style type="text/css">
          #map { 
            height: 500px; 
          }
        </style>
      </head>
    
      <body>
        <div id="map"></div>

        <script src="//unpkg.com/leaflet@1.0.2/dist/leaflet.js"></script>    
        <script>
          var map = L.map('map').setView([51.505, -0.09], 13);
    
          L.tileLayer('http://{s}.tile.osm.org/{z}/{x}/{y}.png', {
              attribution: '&copy; <a href="http://osm.org/copyright">OpenStreetMap</a> contributors'
          }).addTo(map);
          
          L.marker([51.5, -0.09]).addTo(map)
              .bindPopup('A pretty CSS3 popup.<br> Easily customizable.')
              .openPopup();
        </script>
      </body>
    
    </html>


