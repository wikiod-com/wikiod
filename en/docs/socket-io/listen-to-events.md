---
title: "Listen to Events"
slug: "listen-to-events"
draft: false
images: []
weight: 9979
type: docs
toc: true
---

## Listening to internal and custom events:
**`Server Syntax`**
    
    var io = require('socket.io')(80);
    
    io.on('connection', function (mysocket) {

      //custom event called `private message`
      mysocket.on('private message', function (from, msg) {
        console.log('I received a private message by ', from, ' saying ', msg);
      });
    
      //internal `disconnect` event fired, when a socket disconnects
      mysocket.on('disconnect', function () {
        console.log('user disconnected');
      });
    });

**`Client syntax`**:

    var mysocket = io('http://example.com');
      mysocket.on('private message', function (data) {
        console.log(data);
    });

