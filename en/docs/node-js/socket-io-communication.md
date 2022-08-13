---
title: "Socket.io communication"
slug: "socketio-communication"
draft: false
images: []
weight: 9953
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
    const server = app.listen(3000,console.log("Socket.io Hello World server started!"));
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
        <p>Socket.io Hello World client started!</p>
        <p id="message"></p>
      </body>
    </html>

