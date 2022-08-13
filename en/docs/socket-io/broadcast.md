---
title: "Broadcast"
slug: "broadcast"
draft: false
images: []
weight: 9988
type: docs
toc: true
---

## Broadcasting to all users
It is possible to send a message or data to all avaible connections. This can be achieved by first initializing the server and then using the socket.io object to find all sockets and then emit as you normally would emit to a single socket

        
    var io = require('socket.io')(80) // 80 is the HTTP port
    io.on('connection', function (socket) {
          //Callback when a socket connects
        );
    io.sockets.emit('callbackFunction',data); 

## Broadcast to all other sockets
It is possible to emit a message or data to all users except the one making the request:   


    var io = require('socket.io')(80);
    io.on('connection', function (socket) {
      socket.broadcast.emit('user connected');
    });

