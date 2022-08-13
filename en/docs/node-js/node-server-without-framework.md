---
title: "Node server without framework"
slug: "node-server-without-framework"
draft: false
images: []
weight: 9964
type: docs
toc: true
---

Though [Node][1] has many framework to help you getting your server up and running, mainly:

[Express][2]: The most used framework

[Total][3]: The ALL-IN-ONE UNITY framework, that have everything and do not depend on any other framework or module.

But, there is always no one size fits all, so developer may need to build his/her own server, without any other dependency.

If the app i accessed through external server, [CORS][4] could be an issue, a code to avoid it had been provided.


  [1]: https://nodejs.org/en/
  [2]: http://expressjs.com/
  [3]: https://www.totaljs.com/
  [4]: https://developer.mozilla.org/en-US/docs/Web/HTTP/Access_control_CORS

## Framework-less node server
    var http = require('http');
    var fs = require('fs');
    var path = require('path');

    http.createServer(function (request, response) {
    console.log('request ', request.url);

    var filePath = '.' + request.url;
    if (filePath == './')
        filePath = './index.html';

    var extname = String(path.extname(filePath)).toLowerCase();
    var contentType = 'text/html';
    var mimeTypes = {
        '.html': 'text/html',
        '.js': 'text/javascript',
        '.css': 'text/css',
        '.json': 'application/json',
        '.png': 'image/png',
        '.jpg': 'image/jpg',
        '.gif': 'image/gif',
        '.wav': 'audio/wav',
        '.mp4': 'video/mp4',
        '.woff': 'application/font-woff',
        '.ttf': 'applilcation/font-ttf',
        '.eot': 'application/vnd.ms-fontobject',
        '.otf': 'application/font-otf',
        '.svg': 'application/image/svg+xml'
    };

    contentType = mimeTypes[extname] || 'application/octect-stream';

    fs.readFile(filePath, function(error, content) {
        if (error) {
            if(error.code == 'ENOENT'){
                fs.readFile('./404.html', function(error, content) {
                    response.writeHead(200, { 'Content-Type': contentType });
                    response.end(content, 'utf-8');
                });
            }
            else {
                response.writeHead(500);
                response.end('Sorry, check with the site admin for error: '+error.code+' ..\n');
                response.end();
            }
        }
        else {
            response.writeHead(200, { 'Content-Type': contentType });
            response.end(content, 'utf-8');
        }
     });

    }).listen(8125);
    console.log('Server running at http://127.0.0.1:8125/');

## Overcoming CORS Issues
    // Website you wish to allow to connect to
    response.setHeader('Access-Control-Allow-Origin', '*');

    // Request methods you wish to allow
    response.setHeader('Access-Control-Allow-Methods', 'GET, POST, OPTIONS, PUT, PATCH, DELETE');

    // Request headers you wish to allow
    response.setHeader('Access-Control-Allow-Headers', 'X-Requested-With,content-type');

    // Set to true if you need the website to include cookies in the requests sent
    // to the API (e.g. in case you use sessions)
    response.setHeader('Access-Control-Allow-Credentials', true);

