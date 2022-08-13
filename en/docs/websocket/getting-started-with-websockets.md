---
title: "Getting started with WebSockets"
slug: "getting-started-with-websockets"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## web-socket Client example
This is an example of a web-socket client in javascript.  
It:

 1. Connects to a live demo server.
 2. Sends a message.
 3. Receives message(s).
 4. Disconnects after an interval.

<!-- language: lang-js -->

    var mySocket    = null;
    var serverUrl   = 'wss://echo.websocket.org';  //  wss: is ws: but using SSL.
    var oWebSocket  = window.WebSocket || window.MozWebSocket;
    if (oWebSocket) {
        mySocket = new oWebSocket (serverUrl);
        if (mySocket) {
            console.log (mySocket);
            mySocket.onopen     = onSocketOpen;
            mySocket.onclose    = onSocketClose;
            mySocket.onmessage  = onSocketMessage;
            mySocket.onerror    = onSocketError;

            setTimeout (closeSocket, 5000);  //  Be polite and free socket when done.
        }
    }

    function onSocketOpen (evt) {
        console.log ("Socket is now open.");
        mySocket.send ("Hello from my first live web socket!");
    }

    function onSocketClose (evt) {
        console.log ("Socket is now closed.");
    }

    function onSocketMessage (evt) {
        console.log ("Recieved from socket: ", evt.data);
    }

    function onSocketError (evt) {
        console.log ("Error with/from socket!:");
        console.log (evt);
    }

    function closeSocket () {
        if (mySocket.readyState !== mySocket.CLOSED) {
            console.log ("Closing socket from our end (timer).");
            mySocket.close ();
        }
        else
            console.log ("Socket was already closed (timer).");
    }


----------
If you run that code in a console you get a result like:

    WebSocket { url: "wss://echo.websocket.org/", readyState: 0, bufferedAmount: 0, onopen: null,
        onerror: null, onclose: null, extensions: "", protocol: "", onmessage: null,
        binaryType: "blob" 
    }
    2  //  <--- 2 was the socket ID in this instance.
    Socket is now open. 
    Recieved from socket:  Hello from my first live web socket! 
    Closing socket from our end (timer). 
    Socket is now closed.


## Installation or Setup
Detailed instructions on getting web-socket set up or installed.

