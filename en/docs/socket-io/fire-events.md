---
title: "Fire Events"
slug: "fire-events"
draft: false
images: []
weight: 9991
type: docs
toc: true
---

## Fire Custom Events
**`Server syntax`**:

    var io = require('socket.io')(80);
    io.on('connection', function (mysocket) {
      //emit to all but the one who started it
      mysocket.broadcast.emit('user connected');

      //emit to all sockets
      io.emit('my event', { messg: 'for all'});
    });
    
    // a javascript client would listen like this:
    // var mysocket = io('http://example.com');
    // mysocket.on('my event', function (data) {
    //   console.log(data);
    // });
    

**`Client syntax`**:

    var mysocket = io('http://example.com');
    mysocket.emit('another event', { messg: 'hello' });

    // a node.js server would listen like this:
    // require('socket.io')(80).on('connection', function (mysocket) {
    //   mysocket.on('another event', function (data) {
    //     console.log('data from client : '+ data);
    //   });
    // });
    

