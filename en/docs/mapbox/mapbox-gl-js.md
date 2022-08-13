---
title: "Mapbox GL JS"
slug: "mapbox-gl-js"
draft: false
images: []
weight: 9988
type: docs
toc: true
---

## Display a map
<!-- language: lang-html -->
    <!DOCTYPE html>
    <html>
    <head>
        <meta charset='utf-8' />
        <title></title>
        <meta name='viewport' content='initial-scale=1,maximum-scale=1,user-scalable=no' />
        <!-- find the latest library and style versions here: https://www.mapbox.com/mapbox-gl-js/api/ -->
        <script src='https://api.tiles.mapbox.com/mapbox-gl-js/v0.21.0/mapbox-gl.js'></script>
        <link href='https://api.tiles.mapbox.com/mapbox-gl-js/v0.21.0/mapbox-gl.css' rel='stylesheet' />
        <style>
            body { margin:0; padding:0; }
            #map { position:absolute; top:0; bottom:0; width:100%; }
        </style>
    </head>
    <body>
    
    <div id='map'></div>
    <script>
    mapboxgl.accessToken = '<your access token here>';
    var map = new mapboxgl.Map({
        container: 'map', // container id
        style: 'mapbox://styles/mapbox/streets-v9', // style location
        center: [-74.50, 40], // starting position
        zoom: 9 // starting zoom
    });
    </script>
    
    </body>
    </html>



