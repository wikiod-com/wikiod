---
title: "ArcGIS JS API"
slug: "arcgis-js-api"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

**Build Compelling 2D Web Mapping Apps**

The ArcGIS API for JavaScript is designed to maximize your productivity for building engaging, beautiful web mapping applications. The API combines modern web technology and powerful geospatial capabilities enabling you to create high-performing apps and smarter visualizations of your data.

## Loading ArcGIS JS API CDN
Reference the ArcGIS JavaScript API from our CDN and you are ready to get started:

<link rel="stylesheet" href="https://js.arcgis.com/3.19/esri/css/esri.css">
<script src="https://js.arcgis.com/3.19/"></script>

## Loading Map
    require(["esri/map", "dojo/domReady!"], function(Map) {
      var map = new Map("map", {
        center: [-118, 34.5],
        zoom: 8,
        basemap: "topo"
      });
    });

## Loading ESRI CSS
Starting with version 3.2 of the ArcGIS API for JavaScript, developers must include an additional Cascading Style Sheet (CSS) file:  esri.css.

The URL for this file is:

    // versions 3.11 and forward
    <link rel="stylesheet" href="https://js.arcgis.com/API-VERSION/esri/css/esri.css">
    
    // versions prior to 3.11
    <link rel="stylesheet" href="https://js.arcgis.com/API-VERSION/js/esri/css/esri.css">
   
 For instance, this CSS file would be included via a link tag:
    
    <link rel="stylesheet" href="https://js.arcgis.com/3.19/esri/css/esri.css">

The esri.css file contains the CSS for various widgets, including CSS for the map.

 Because all CSS is in a single file, retrieving CSS for the API is done in a single request. Reducing the number of http requests is one way for apps to improve performance.


## Loading Dojo specific CSS

The esri.css file does not include the CSS for various Dojo widgets or themes like tundra or claro; those files must be included separately. Exceptions are the Grid and RangeSlider, which are used by widgets in the API. Grid styles must be explicitly included.

For instance, this CSS file would be included via an additional link tag:

    <link rel="stylesheet" href="https://js.arcgis.com/3.19/dijit/themes/claro/claro.css">

**Legacy CSS**

Prior to version 3.2, CSS files were appended to a page's DOM dynamically via JavaScript.

 Dynamically appending CSS files makes overriding default styles cumbersome because CSS included via a link or style tag is parsed before JavaScript appends CSS to a page. This results in having to use  !important to override default widget styles. Because widgets have been refactored to not dynamically append their CSS, !important is no longer necessary. API versions prior to 3.2 have not been modified so it is still necessary to use !important with older versions. Explicitly including CSS via a link tag instead of dynamically appending CSS also follows the convention used by components that ship with Dojo.

