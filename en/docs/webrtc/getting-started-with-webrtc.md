---
title: "Getting started with webrtc"
slug: "getting-started-with-webrtc"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Setting up a WebRTC-based communication system
To setup a WebRTC-based communication system, you need three main components:

 1. **A WebRTC signaling server**

    To establish a WebRTC connections, peers need to contact a signaling server, which then provides the address information the peers require to set up a peer-to-peer connection. Signaling servers are for example:

    - [signalmaster](https://github.com/andyet/signalmaster): Lightweight, JavaScript-based signaling server
    - [NextRTC](https://nextrtc.org/): Java-based signaling server
    - [Kurento](https://www.kurento.org/): Comprehensive WebRTC framework
    - [Janus](https://janus.conf.meetecho.com/): General purpose WebRTC Gateway

 2. **A WebRTC client application**

    The client accesses either a browser's WebRTC implementation through a JavaScript API or uses a WebRTC library (i.e. as part of a desktop or mobile app). To establish the connection to a peer, the client first needs to connect to the signaling server. Examples for WebRTC clients are:

     - [Several Kurento projects](https://github.com/kurento)
   
     - [OpenWebRTC](http://www.openwebrtc.org/), a cross-platform client with mobile focus

     - [Peer.js](http://peerjs.com/) A browser-based client (Peer.js also provides a light-weight server)
     - [Janus Demo examples](https://janus.conf.meetecho.com/demos.html)

 3. **A STUN/TURN server**

    [Session Traversal Utilities for NAT (STUN)](https://de.wikipedia.org/wiki/Session_Traversal_Utilities_for_NAT) enables peers to exchange address information even if they are behind routers employing [Network Adress Translation (NAT)](https://en.wikipedia.org/wiki/Network_address_translation). If network restrictions prevent peers from communication directly at all, the traffic is routed via a [Traversal Using Relays around NAT (TURN)](https://en.wikipedia.org/wiki/Traversal_Using_Relays_around_NAT) server. You find a detailed and graphical explanation of STUN and TURN at http://www.avaya.com/blogs/archives/2014/08/understanding-webrtc-media-connections-ice-stun-and-turn.html. Examples for WebRTC STUN/TURN servers are:
    - [coturn](https://github.com/coturn/coturn) combines STUN and TURN and is typically part of a fully-fledged WebRTC infrastructure.
    - [Janus WebRTC Gateway][1] comes with an integrated STUN/TURN server.
  


  [1]: https://github.com/meetecho/janus-gateway

## Introduction to WebRTC
WebRTC is an open framework for the web that enables Real Time Communications in the browser. It includes the fundamental building blocks for high-quality communications on the web, such as network, audio and video components used in voice and video chat applications.

These components, when implemented in a browser, can be accessed through a JavaScript API, enabling developers to easily implement their own RTC web app.

The WebRTC effort is being standardized on an API level at the [W3C](https://www.w3.org/TR/webrtc/) and at the protocol level at the [IETF](https://tools.ietf.org/wg/rtcweb/).

 - A key factor in the success of the web is that its core technologies –
   such as HTML, HTTP, and TCP/IP – are open and freely implementable.
   Currently, there is no free, high-quality, complete solution
   available that enables communication in the browser. WebRTC enables
   this.
 - Already integrated with best-of-breed voice and video engines that
   have been deployed on millions of endpoints over the last 8+ years.
   Google does not charge royalties for WebRTC.
 - Includes and abstracts key NAT and firewall traversal technology,
   using STUN, ICE, TURN, RTP-over-TCP and support for proxies.
 - Builds on the strength of the web browser: WebRTC abstracts signaling
   by offering a signaling state machine that maps directly to
   PeerConnection. Web developers can therefore choose the protocol of
   choice for their usage scenario (for example, but not limited to,
   SIP, XMPP/Jingle, etc).

Read more about WebRTC from [here][1]


  [1]: https://en.wikipedia.org/wiki/WebRTC

## Get access to your audio and video using getUserMedia() API, Hello WebRTC!
`navigator.mediaDevices` is the common method adapted in Chrome and FF to getUserMedia as of now.

A promised based call back which returns local stream on success

    navigator.mediaDevices.getUserMedia({ audio: true, video: true })
        .then(stream => {
            // attach this stream to window object so you can reuse it later
            window.localStream = stream;
            // Your code to use the stream
        })
        .catch((err) =>{
            console.log(err);
        });

You can pass audio and video [constraints][1] to getUserMedia to control capture settings like resolution, framerate, device preference, and more.

Attach the stream to a video element

    // Set the video element to autoplay to ensure the video appears live
    videoElement.autoplay = true;
    // Mute the local video so the audio output doesn't feedback into the input
    videoElement.muted = true;
    // attach the stream to the video element

stop both video and audio

    localStream.getTracks().forEach((track) => {
        track.stop();
    });

stop only audio

    localStream.getAudioTracks()[0].stop();

stop only video 

    localStream.getVideoTracks()[0].stop();

[Live demo][2]


  [1]: https://developer.mozilla.org/en-US/docs/Web/API/MediaDevices/getUserMedia
  [2]: https://webrtc.github.io/samples/src/content/getusermedia/gum/

