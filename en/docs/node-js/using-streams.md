---
title: "Using Streams"
slug: "using-streams"
draft: false
images: []
weight: 9911
type: docs
toc: true
---

## Parameters
| Parameter | Definition |
| ------ | ------ |
| Readable Stream   | type of stream where data can be read from |
| Writable Stream | type of stream where data can be written to
| Duplex Stream | type of stream that is both readable and writeable
| Transform Stream | type of duplex stream that can transform data as it is being read and then written

## Read Data from TextFile with Streams
I/O in node is asynchronous, so interacting with the disk and network involves passing callbacks to functions. You might be tempted to write code that serves up a file from disk like this:


    var http = require('http');
    var fs = require('fs');
    
    var server = http.createServer(function (req, res) {
        fs.readFile(__dirname + '/data.txt', function (err, data) {
            res.end(data);
        });
    });
    server.listen(8000);

This code works but it's bulky and buffers up the entire data.txt file into memory for every request before writing the result back to clients. If data.txt is very large, your program could start eating a lot of memory as it serves lots of users concurrently, particularly for users on slow connections.

The user experience is poor too because users will need to wait for the whole file to be buffered into memory on your server before they can start receiving any contents.

Luckily both of the (req, res) arguments are streams, which means we can write this in a much better way using fs.createReadStream() instead of fs.readFile():

    var http = require('http');
    var fs = require('fs');
    
    var server = http.createServer(function (req, res) {
        var stream = fs.createReadStream(__dirname + '/data.txt');
        stream.pipe(res);
    });
    server.listen(8000);

Here .pipe() takes care of listening for 'data' and 'end' events from the fs.createReadStream(). This code is not only cleaner, but now the data.txt file will be written to clients one chunk at a time immediately as they are received from the disk.



## Piping streams
Readable streams can be "piped," or connected, to writable streams. This makes data flow from the source stream to the destination stream without much effort.

    var fs = require('fs')

    var readable = fs.createReadStream('file1.txt')
    var writable = fs.createWriteStream('file2.txt')

    readable.pipe(writable) // returns writable

When writable streams are also readable streams, i.e. when they're *duplex* streams, you can continue piping it to other writable streams.

    var zlib = require('zlib')

    fs.createReadStream('style.css')
      .pipe(zlib.createGzip()) // The returned object, zlib.Gzip, is a duplex stream.
      .pipe(fs.createWriteStream('style.css.gz')

Readable streams can also be piped into multiple streams.

    var readable = fs.createReadStream('source.css')
    readable.pipe(zlib.createGzip()).pipe(fs.createWriteStream('output.css.gz'))
    readable.pipe(fs.createWriteStream('output.css')

Note that you must pipe to the output streams synchronously (at the same time) before any data 'flows'. Failure to do so might lead to incomplete data being streamed.

Also note that stream objects can emit `error` events; be sure to responsibly handle these events on _every_ stream, as needed:

    var readable = fs.createReadStream('file3.txt')
    var writable = fs.createWriteStream('file4.txt')
    readable.pipe(writable)
    readable.on('error', console.error)
    writable.on('error', console.error)


## Creating your own readable/writable stream
We will see stream objects being returned by modules like fs etc but what if we want to create our own streamable object.

To create Stream object we need to use the stream module provided by NodeJs

        var fs = require("fs");
        var stream = require("stream").Writable;
        
        /* 
         *  Implementing the write function in writable stream class.
         *  This is the function which will be used when other stream is piped into this 
         *  writable stream.
         */
        stream.prototype._write = function(chunk, data){
            console.log(data);
        }
        
        var customStream = new stream();
        
        fs.createReadStream("am1.js").pipe(customStream);

This will give us our own custom writable stream. we can implement anything within         the *_write* function.
Above method works in NodeJs 4.x.x version but in NodeJs 6.x **ES6** introduced classes
therefore syntax have changed. Below is the code for 6.x version of NodeJs

        const Writable = require('stream').Writable;
        
        class MyWritable extends Writable {
          constructor(options) {
            super(options);
          }
        
          _write(chunk, encoding, callback) {
            console.log(chunk);
          }
        }

## Why Streams?


