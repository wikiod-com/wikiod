---
title: "How does ExpressJs work"
slug: "how-does-expressjs-work"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

## Middlewares Stack
A basic middleware is a function that takes 3 arguments **request**, **response** and **next**. 
 
Then by `app.use`, a middleware is mounted to the Express App Middlewares Stack. Request and response are manipulated in each middleware then piped to the next one through the call of `next()`.  

For example, the below code:  

    var express = require('express');
    var app = express();

    app.use((request, response, next) => {        
        request.propA = "blah blah"; 
        next(); 
    });

    app.use('/special-path', (request, response, next) => {
        request.propB = request.propA + " blah";
        if (request.propB === "blah blah blah")
            next();
        else
            response.end('invalid');
    });

    app.use((request, response, next) => {
        response.end(request.propB);
    });

    app.listen(1337);

Can roughly be translated to:

    var http = require('http');
    http.createServer((request, response) => {
        
        //Middleware 1
        if (isMatch(request.url, '*')) {
            request.propA = "blah blah";
        }

        //Middleware 2
        if (isMatch(request.url, "/special-path")) {
            request.propB = request.propA + " blah";
            if (request.propB !== "blah blah blah")
                return response.end('invalid');
        }

        //Middleware 3
        if (isMatch(request.url, "*")) {
            return response.end(request.propB);
        }
    });

    server.listen(1337);



## Handling request/response
# The syntactic sugar
Most of the getting started examples of ExpressJs include this piece of code

    var express = require('express');
    var app = express();
    ...
    app.listen(1337);

Well, `app.listen` is just a shortcut for:

    var express = require('express');
    var app = express();
    var http = require('http');
    http.createServer(app).listen(1337);

# The Express App
The famous `http.createServer` accept a function which is known as the handler.  The handler takes 2 parameters **request** and **response** as inputs, then manipulating them inside it's scope to do various things.  

So basically `app = express()` is a function, taking place as the handler and dealing with request, response through a set of special components referred as middlewares.



