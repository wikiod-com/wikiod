---
title: "Getting started with socket.io"
slug: "getting-started-with-socketio"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## "Hello world!" with socket messages.
Install node modules

    npm install express
    npm install socket.io

Node.js server

    const express = require('express'); 
    const app = express();
    const server = app.listen(3000,console.log("Socket.io Hello Wolrd server started!"));
    const io = require('socket.io')(server);
    
    io.on('connection', (socket) => {
        //console.log("Client connected!");
        socket.on('message-from-client-to-server', (msg) => {
            console.log(msg);
        })
        socket.emit('message-from-server-to-client', 'Hello World!');
    });

Browser client

    <!DOCTYPE html>
    <html lang="en">
      <head>
        <meta charset="UTF-8">
        <title>Hello World with Socket.io</title>
      </head>
      <body>
        <script src="https://cdn.socket.io/socket.io-1.4.5.js"></script>
        <script>
          var socket = io("http://localhost:3000");
          socket.on("message-from-server-to-client", function(msg) {
              document.getElementById('message').innerHTML = msg;
          });
          socket.emit('message-from-client-to-server', 'Hello World!');
        </script>
        <p>Socker.io Hello World client started!</p>
        <p id="message"></p>
      </body>
    </html>

In the above example, the path to the socket.io library is defined as `/socket.io/socket.io.js`.

Even though we didn't write any code to serve the socket.io library, Socket.io automatically does that.

## Installation or Setup
First, install `socket.io` module in `node.js` application.

    npm install socket.io --save

**Basic HTTP Setup**

The following example attaches `socket.io` to a plain `node.js` HTTP server listening on port 3000.

    var server = require('http').createServer();

    var io = require('socket.io')(server);

    io.on('connection', function(socket){

      console.log('user connected with socketId '+socket.id);

      socket.on('event', function(data){
          console.log('event fired');
      });

      socket.on('disconnect', function(){
          console.log('user disconnected');
      });

    });

    server.listen(3000);


**Setup with Express**

Express app can be passed to `http` server which will be attached to `socket.io`. 

    var app = require('express')();                   //express app
    var server = require('http').createServer(app);   //passed to http server
    var io = require('socket.io')(server);            //http server passed to socket.io
    
    io.on('connection', function(){

      console.log('user connected with socketId '+socket.id);

      socket.on('event', function(data){
          console.log('event fired');
      });

      socket.on('disconnect', function(){
          console.log('user disconnected');
      });

    });

    server.listen(3000);

**Client Side Setup**

Check the Hello World example above for the client side implementation.

