---
title: "Simulating Location Using GPX files iOS"
slug: "simulating-location-using-gpx-files-ios"
draft: false
images: []
weight: 9994
type: docs
toc: true
---

## Your .gpx file: MPS_HQ.gpx
<?xml version="1.0" encoding="UTF-8" standalone="no"?>
    <gpx xmlns="http://www.topografix.com/GPX/1/1"
     xmlns:gpxx = "http://www.garmin.com/xmlschemas/GpxExtensions/v3"
     xmlns:xsi = "http://www.w3.org/2001/XMLSchema-instance"
     xsi:schemaLocation="http://www.topografix.com/GPX/1/1 
     http://www.topografix.com/GPX/1/1/gpx.xsd
     http://www.garmin.com/xmlschemas/GpxExtensions/v3
     http://www8.garmin.com/xmlschemas/GpxExtensions/v3/GpxExtensionsv3.xsd"
     version="1.1"
     creator="gpx-poi.com">
     <wpt lat="38.9072" lon="77.0369">38.9072/-77.0369
     <time>2015-04-16T22:20:29Z</time>
      <name>Washington, DC</name>
      <extensions>
         <gpxx:WaypointExtension>
            <gpxx:Proximity>10</gpxx:Proximity>
            <gpxx:Address>
               <gpxx:StreetAddress>Washington DC</gpxx:StreetAddress>
               <gpxx:City>Washington</gpxx:City>
               <gpxx:State>DC</gpxx:State>
               <gpxx:Country>United States</gpxx:Country>
               <gpxx:PostalCode> 20005 </gpxx:PostalCode>
            </gpxx:Address>
         </gpxx:WaypointExtension>
      </extensions>
   </wpt>
</gpx>

## To set this location:
 1. Go to Edit Scheme.
 2. Select Run -> Options.
 3. Check "Allow Location Simulation".
 4. Select the *.GPX File Name from the "Default Location" drop down list.

[![Allow Location Simulation][1]][1]


  [1]: https://i.stack.imgur.com/NxJyr.png

