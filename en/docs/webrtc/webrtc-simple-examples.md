---
title: "WebRTC simple examples"
slug: "webrtc-simple-examples"
draft: false
images: []
weight: 9983
type: docs
toc: true
---

## Parameters

| getUserMedia() Paramters| Description |
| ------ | ------ |
| Constraints   | It consist of array which allows us to specify which media devices to use i.e audio or video or both   |
|Success callback|Create a function for success callback which will provide you the stream which you get from your media devices|
|Error callback|This callback get invoked when there is problem like there are no media devices, or user has denied the permission to use them|



## Get camera and microphone permission and display preview on webpage
In order to begin using WebRTC you need to get camera and microphone permission.For that you need following things: 
1. `adapter.js`, you can get it from [here][1]
2. A html webpage with a video tag and little bit of js code

The adapter.js is a JavaScript shim for WebRTC, maintained by Google with help from the [WebRTC community][2], that abstracts vendor prefixes, browser differences and spec changes.  

Now once you have this file, create a html file with following code:  

    <!DOCTYPE html>
    <html>
        <head>
            <title>My first webrtc example</title>
            <script src="adapter.js"></script>
            <script type="text/javascript">
                function gotStream(stream) {
                    window.AudioContext = window.AudioContext || window.webkitAudioContext;
                    var audioContext = new AudioContext();
    
                    // Create an AudioNode from the stream
                    var mediaStreamSource = audioContext.createMediaStreamSource(stream);
    
                    // Connect it to destination to hear yourself
                    // or any other node for processing!
                    mediaStreamSource.connect(audioContext.destination);
                    var video = document.querySelector('video');
                    var videoTracks = stream.getVideoTracks();
                    window.stream = stream; // make variable available to browser console
                    video.srcObject = stream;
                }
                function onfail(error) {
                    console.log("permission not granted or system don't have media devices."+error.name);
                }
                navigator.getUserMedia({audio:true,video:true}, gotStream,onfail);
                
                
            </script>
        </head>
        <body>
            Welcome to webrtc
            <video id="local" autoplay=""></video>
        </body>
    </html>

Once done, save this file and run in the browser. When you run the browser will ask you to allow this webpage to access your webcam and microphone. Allow it and whola!, you will see the preview on the webpage.

  [1]: https://github.com/webrtc/samples/blob/gh-pages/src/js/adapter.js
  [2]: https://github.com/webrtc/adapter/graphs/contributors

