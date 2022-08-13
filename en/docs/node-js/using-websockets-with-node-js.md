---
title: "Using WebSocket's with Node.JS"
slug: "using-websockets-with-nodejs"
draft: false
images: []
weight: 9964
type: docs
toc: true
---

## Installing WebSocket's
There are a few way's to install WebSocket's to your project. Here are some example's:

    npm install --save ws

or inside your package.json using:

    "dependencies": {
      "ws": "*"
    },



## Adding WebSocket's to your file's
To add ws to your file's simply use:

    var ws = require('ws');

## Using WebSocket's and WebSocket Server's
To open a new WebSocket, simply add something like:

    var WebSocket = require("ws");
    var ws = new WebSocket("ws://host:8080/OptionalPathName);
    // Continue on with your code...

Or to open a server, use:

    var WebSocketServer = require("ws").Server;
    var ws = new WebSocketServer({port: 8080, path: "OptionalPathName"});


    

## A Simple WebSocket Server Example
    var WebSocketServer = require('ws').Server
    , wss = new WebSocketServer({ port: 8080 }); // If you want to add a path as well, use path: "PathName"

    wss.on('connection', function connection(ws) {
      ws.on('message', function incoming(message) {
        console.log('received: %s', message);
      });

      ws.send('something');
    });

