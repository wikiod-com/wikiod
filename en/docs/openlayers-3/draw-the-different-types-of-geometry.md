---
title: "Draw the different types of geometry"
slug: "draw-the-different-types-of-geometry"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

## Draw a Multi Line Geometry
## Create a vector source

    var vectorSource = new ol.source.Vector({});

## Initiate Map Object and add vector Layer to the map and Source as the vectorSource

    var map = new ol.Map({
      layers: [
          new ol.layer.Tile({
          source: new ol.source.OSM()
          }),
          new ol.layer.Vector({
              source: vectorSource
          })
      ],
      target: 'map',
      view: new ol.View({
        center: [45, 5],
        zoom:5
      })
    });

## Transform the projection from source projection system to target project system. 

    var points=[];
    for (i = 0; i < 10; i++) { 
        var xx = Math.random() * (xmax - xmin) + xmin;
        var yy = Math.random() * (ymax - ymin) + ymin;
        points.push(ol.proj.transform([xx,yy],'EPSG:4326', 'EPSG:3857'));
    }

## pass points to the ol.geom.MultiLineString([]) constructor

    var thing = new ol.geom.MultiLineString([points1]);

## Create a feature and add geometry as a thing

    var featurething = new ol.Feature({
        name: "Thing",
        geometry: thing,
        style : new ol.style.Style({
          stroke : new ol.style.Stroke({
            color : 'red'
          })
        })
    });

## Finally add it to source

    vectorSource.addFeature( featurething );

Note : It is very important to put proper source and target projection systems

