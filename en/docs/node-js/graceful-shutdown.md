---
title: "Graceful Shutdown"
slug: "graceful-shutdown"
draft: false
images: []
weight: 9983
type: docs
toc: true
---

## Graceful Shutdown - SIGTERM
By using **server.close()** and **process.exit()**, we can catch the server exception and do a graceful shutdown.

    var http = require('http');
    
    var server = http.createServer(function (req, res) {
      setTimeout(function () { //simulate a long request
        res.writeHead(200, {'Content-Type': 'text/plain'});
        res.end('Hello World\n');
      }, 4000);
    }).listen(9090, function (err) {
      console.log('listening http://localhost:9090/');
      console.log('pid is ' + process.pid);
    });
    
    process.on('SIGTERM', function () {
      server.close(function () {
        process.exit(0);
      });
    });

