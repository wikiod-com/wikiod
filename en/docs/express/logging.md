---
title: "Logging"
slug: "logging"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

`morgan` is an HTTP request logger middleware for node.js

## Installation
First, install the `morgan` Middleware in your project

    npm install --save morgan

## Simple Express logging of all request to STDOUT
Add the following code to your `app.js` file:

    var express = require('express')
    var morgan = require('morgan')
    
    var app = express()
    
    app.use(morgan('combined'))
    
    app.get('/', function (req, res) {
      res.send('hello, world!')
    })

Now when you access your website you will see in the console you used to start the server that the requests are logged

## Write Express logs to a rotating log file
First, install `fs`, `file-stream-rotator` and `path` in your project

    npm install --save fs file-stream-rotator path

Add the following code to your `app.js` file:

    var FileStreamRotator = require('file-stream-rotator')
    var express = require('express')
    var fs = require('fs')
    var morgan = require('morgan')
    var path = require('path')
    
    var app = express()
    var logDirectory = path.join(__dirname, 'log')
    
    // ensure log directory exists
    fs.existsSync(logDirectory) || fs.mkdirSync(logDirectory)
    
    // create a rotating write stream
    var accessLogStream = FileStreamRotator.getStream({
      date_format: 'YYYYMMDD',
      filename: path.join(logDirectory, 'access-%DATE%.log'),
      frequency: 'daily',
      verbose: false
    })
    
    // setup the logger
    app.use(morgan('combined', {stream: accessLogStream}))
    
    app.get('/', function (req, res) {
      res.send('hello, world!')
    })

Now when you access your website you will see a `log` directory was created and a log file with a name format of `access-%DATE%.log` was created in your log directory

## Write Express logs to a single file
First, install `fs` and `path` in your project

    npm install --save fs path

Add the following code to your `app.js` file:

    var express = require('express')
    var fs = require('fs')
    var morgan = require('morgan')
    var path = require('path')
    
    var app = express()
    
    // create a write stream (in append mode)
    var accessLogStream = fs.createWriteStream(path.join(__dirname, 'access.log'), {flags: 'a'})
    
    // setup the logger
    app.use(morgan('combined', {stream: accessLogStream}))
    
    app.get('/', function (req, res) {
      res.send('hello, world!')
    })

Now when you access your website you will see a `access.log` file was created in your project directory

