---
title: "Getting started with google-maps-api-3"
slug: "getting-started-with-google-maps-api-3"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Place the user's pin in the map.
----------
**Note**, if you are not familiar with the google maps api, you may read the precedent example (basics) in order to understand this little example.

----------

 

 

- **First, initialize the map**. 

You may add an map's element in your HTML code and a bite of CSS like this:


    <!DOCTYPE html>
    <html>
    <head>
    <style>
      /* Always set the map height explicitly to define the size of the div
       * element that contains the map. */
      #map {
        height: 100%;
      }
      /* Optional: Makes the sample page fill the window. */
      html, body {
        height: 100%;
        margin: 0;
        padding: 0;
      }
    </style>
    </head>
    <body>
    <div id="map"></div>
    </body>
    </html>

Now, you have to add the google maps library into your code with a balise script like this:


    <script src="https://maps.googleapis.com/maps/api/js?key=YOUR_API_KEY&callback=initMap"
    async defer></script>

## You may remplace YOUR_API_KEY in the code by a google api key. The is a [link][1] to get a key. ##

   Next, you have to write in your code a function witch serve as a callback (or a function of initialization) for your map. Here, we just add a small function witch you can find on google [here][2]:

    function initMap() {
        map = new google.maps.Map(document.getElementById('map'), {
          center: {lat: -34.397, lng: 150.644},
          zoom: 8
        });
      }

 ## Now you normally have a basic map on your screen. You can find the complete code on [google][2]. ##


----------


 - **Second, find the user position.**

To  request the user position, there is a very simple function witch is provided by the navigator:

    navigator.geolocation.getCurrentPosition(showPosition);

Note that this function accept a parameter. It is a function to call if the geolocation is successful.

We have to create this function. :)

    function showPosition(position) {
        alert (position);    
    }

This function is very simple and we will have to update it after in order to plot a marker on the user position.

## The geolocation's function witch we use here is very simple. You can have the complete documentation on [w3schools][3]. ##

At his point the code looks like this:

    <!DOCTYPE html>
    <html>

    <head>
      <title>Simple Map</title>
      <meta name="viewport" content="initial-scale=1.0">
      <meta charset="utf-8">
      <style>
        /* Always set the map height explicitly to define the size of the div
          * element that contains the map. */
        
        #map {
          height: 100%;
        }
        /* Optional: Makes the sample page fill the window. */
        
        html,
        body {
          height: 100%;
          margin: 0;
          padding: 0;
        }
      </style>
    </head>

    <body>
      <div id="map"></div>
      <script>
        var map;
        navigator.geolocation.getCurrentPosition(showPosition);
        function initMap() {
          map = new google.maps.Map(document.getElementById('map'), {
            center: { lat: -34.397, lng: 150.644 },
            zoom: 8
          });
        }
        function showPosition(position) {
          console.log(position);
        }
      </script>
      <script src="https://maps.googleapis.com/maps/api/js?key=YOUR_API_KEY&callback=initMap" async defer></script>
    </body>

    </html>


----------


 - **And third, display the user's position on the map with a marker.**

In order to display a marker on the map you can use the function in the example 'basics':

    // Create a marker and place it on the map
    var marker = new google.maps.Marker({
        position: position,
        map: map
    });

I will no details this lines of codes very precisely. You just may to now that when you create a marker with this code: `new google.maps.Marker({});`, you pass the 'marker options' enter the embraces. You can consult the google documentation [here][4].

**Also note** that you can specify the position of the marker very easily with the position parameter.

Now we have to modify the `showPosition` function.

 You can access simply to the lat and lng of the variable position like this:

      var markerPosition={};
      markerPosition.lat=position.coords.latitude;
      markerPosition.lng=position.coords.longitude;

Like this, google understand how to simply access to the lat and lng value.

Now we add to modify the `showPosition` function to add a marker in the user position.

    function showPosition(position) {
        var markerPosition={};
        markerPosition.lat=position.coords.latitude;
        markerPosition.lng=position.coords.longitude;
        // Create a marker and place it on the map
        var marker = new google.maps.Marker({
            position: markerPosition,
            map: map
        });
    }

    


----------


   - Finally, your code should looks like this:

    <!DOCTYPE html>
    <html>
    
    <head>
      <title>Simple Map</title>
      <meta name="viewport" content="initial-scale=1.0">
      <meta charset="utf-8">
      <style>
        /* Always set the map height explicitly to define the size of the div
           * element that contains the map. */
        
        #map {
          height: 100%;
        }
        /* Optional: Makes the sample page fill the window. */
        
        html,
        body {
          height: 100%;
          margin: 0;
          padding: 0;
        }
      </style>
    </head>
    
    <body>
      <div id="map"></div>
      <script>
        var map;
        navigator.geolocation.getCurrentPosition(showPosition);
        function initMap() {
          map = new google.maps.Map(document.getElementById('map'), {
            center: { lat: -34.397, lng: 150.644 },
            zoom: 8
          });
        }
        function showPosition(position) {
          var markerPosition={};
          markerPosition.lat=position.coords.latitude;
          markerPosition.lng=position.coords.longitude;
    
          // Create a marker and place it on the map
          var marker = new google.maps.Marker({
            position: markerPosition,
            map: map
          });
        }
      </script>
      <script src="https://maps.googleapis.com/maps/api/js?key=YOUR_API_KEY&callback=initMap" async defer></script>
    </body>
    
    </html>


  [1]: https://developers.google.com/maps/documentation/javascript/get-api-key?hl=en
  [2]: https://developers.google.com/maps/documentation/javascript/examples/map-simple?hl=en
  [3]: https://www.w3schools.com/html/html5_geolocation.asp
  [4]: https://developers.google.com/maps/documentation/javascript/markers?hl=en

## Basics
# CSS

Here are the minimum CSS rules that Google advises you to use, in a separate CSS file, or within an HTML style tag, e.g. `<style type="text/css">...</style>`.

<!-- language: lang-css -->

    html, body {
        height: 100%; 
        margin: 0; 
        padding: 0;
    }
    
    #map {
        height: 400px;
    }

# HTML

Google recommends that you declare a true `DOCTYPE` within your web application.

<!-- language: lang-html -->

    <!DOCTYPE html>

Use the following script tag to load the Google Maps JavaScript API in your application.

<!-- language: lang-html -->

    <script async defer
      src="https://maps.googleapis.com/maps/api/js?key=YOUR_API_KEY&callback=initialize">
    </script>

Create an HTML element to hold the map.

<!-- language: lang-html -->

    <div id="map"></div>

# JavaScript

Here is a very simple example displaying a [Map](https://developers.google.com/maps/documentation/javascript/3.exp/reference#Map) and a [Marker](https://developers.google.com/maps/documentation/javascript/3.exp/reference#Marker).

<!-- language: lang-js -->

    function initialize() {

        // Create a LatLng object
        // We use this LatLng object to center the map and position the marker
        var center = new google.maps.LatLng(50,0);

        // Declare your map options
        var mapOptions = {
            zoom: 4,
            center: center,
            mapTypeId: google.maps.MapTypeId.ROADMAP
        };

        // Create a map in the #map HTML element, using the declared options
        var map = new google.maps.Map(document.getElementById("map"), mapOptions);

        // Create a marker and place it on the map
        var marker = new google.maps.Marker({
            position: center,
            map: map
        });
    }

# Complete example

<!-- language: lang-html -->

    <!DOCTYPE html>
    <html>
        <head>
            <title>Simple Map</title>
            <meta name="viewport" content="initial-scale=1.0">
            <meta charset="utf-8">
            <style>
                html, body {
                    height: 100%;
                    margin: 0;
                    padding: 0;
                }

                #map {
                    height: 400px;
                }
            </style>
        </head>
        <body>
            <div id="map"></div>
            <script>
                function initialize() {

                    // Create a LatLng object
                    // We use this LatLng object to center the map and position the marker
                    var center = new google.maps.LatLng(50, 0);

                    // Declare your map options
                    var mapOptions = {
                        zoom: 4,
                        center: center,
                        mapTypeId: google.maps.MapTypeId.ROADMAP
                    };

                    // Create a map in the #map HTML element, using the declared options
                    var map = new google.maps.Map(document.getElementById("map"), mapOptions);

                    // Create a marker and place it on the map
                    var marker = new google.maps.Marker({
                        position: center,
                        map: map
                    });
                }
            </script>
            <script src="https://maps.googleapis.com/maps/api/js?key=YOUR_API_KEY&callback=initialize" async defer></script>
        </body>

    </html>


# Demo

<kbd>[JSFiddle demo](http://jsfiddle.net/upsidown/Lw6tF/)</kbd>

# More info



Please read this topic's [Remarks](https://www.wikiod.com/google-maps-api-3/getting-started-with-google-maps-api-3) for more information.

