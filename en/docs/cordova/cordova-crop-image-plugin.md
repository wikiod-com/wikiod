---
title: "Cordova Crop Image Plugin"
slug: "cordova-crop-image-plugin"
draft: false
images: []
weight: 9764
type: docs
toc: true
---

## Crop Image after clicking using camera or selecting image.

It crops the images in square shape. 

This cordova project uses two plugins:

1) Cordova Camera Plugin -- https://github.com/apache/cordova-plugin-camera

2) Cordova Crop Image Plugin -- https://github.com/jeduan/cordova-plugin-crop


The Camera plugin is combined with the Crop Image Plugin by putting the Cop Image Plugin Code within the success callback of Camera Plugin Code.
      
      /*Camera Plugin Code*/
      navigator.camera.getPicture(onSuccess, onFail, {
          quality: 50,
          destinationType: Camera.DestinationType.FILE_URI
      });

      function onSuccess(imageData) {
           console.log(imageData);
           
           /*Crop Image Plugin Code*/
           plugins.crop(function success (data) {
              console.log(data);
              var image = document.getElementById('myImage');
              image.src = data;
           }, 
           function fail () {

           }, imageData, {quality:100});
      }

     function onFail(message) {
        alert('Failed because: ' + message);
     }


<a href="http://imgur.com/1xPUR9h"><img src="http://i.imgur.com/1xPUR9h.png?1" title="source: imgur.com" /></a>
<a href="http://imgur.com/PPSln16"><img src="http://i.imgur.com/PPSln16.png?1" title="source: imgur.com" /></a>
<br>
<a href="http://imgur.com/wcsHbKN"><img src="http://i.imgur.com/wcsHbKN.png?3" title="source: imgur.com" /></a>
<a href="http://imgur.com/ILag71Z"><img src="http://i.imgur.com/ILag71Z.png?2" title="source: imgur.com" /></a>



