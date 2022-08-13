---
title: "Connect"
slug: "connect"
draft: false
images: []
weight: 9987
type: docs
toc: true
---

## Connect and Express
Express is based on Connect, which is what provides the middleware functionality of Express. To understand what connect is, you can see that it provides the basic app structure that you use when you use express

    const connect = require('connect')
    
    const app = connect()
    app.listen(3000)
   
This will open a "empty" http server that will respond 404 to all requests.

## Middleware
Middleware are attached to the app object, usually before listen is called. Example of a simple logging middleware:

    app.use(function (req, res, next) {
        console.log(`${req.method}: ${req.url}`)
        next()
    })
    
All this will do is log `GET: /example` if you where to GET `localhost:3000/example`. All requests will still return 404 since you are not responding with any data.

The next middleware in the chain will be run as soon as the previous one calls `next()`, so we can go ahead and respond to the requests by adding yet another middleware like this:

    app.use(function (req, res, next) {
        res.end(`You requested ${req.url}`)
    })

Now when you request ´localhost:3000/example` you will be greeted with "You requested /example". There is no need to call `next` this time since this middleware is the last in the chain (but nothing bad will happen if you did),

Complete program this far:

    const connect = require('connect')
    
    const app = connect()

    app.use(function (req, res, next) {
        console.log(`${req.method}: ${req.url}`)
        next()
    })
    
    app.use(function (req, res, next) {
        res.end(`You requested ${req.url}`)
        next()
    })
    
    app.listen(3000)
    

## Errors and error middleware
If we would like to limit the access to our app, we could write a middleware for that too! This example only grants you access on thrusdays, but a real world example could, for example, be _user authentication_. A good place to put this would be after the logging middleware but before any content is sent.

    app.use(function (req, res, next) {
        if (new Date().getDay() !== 4) {
            next('Access is only granted on thursdays')
        } else {
            next()
        }
    })

As you can see in this example, sending an error is as easy as providing a prameter to the `next()` function.

Now, if we visit the website on any day different than a thursday we would be greeted with a 500 error and the string `'Access is only granted on thursdays'`.

Now, this isn't good enough for our site. We would rather send the user a HTML message in another middleware:

    app.use(function (err, req, res, next) {
        res.end(`<h1>Error</h1><p>${err}</p>`)
    })
    
This works kind of like a catch block: any error in the middleware prior to the error middleware will be sent to the former. An error middleware is identified by its 4 parameters.

You could also use the error middleware to recover from the error by calling the next method again:

    app.use(function (err, req, res, next) {
        // Just joking, everybody is allowed access to the website!
        next()
    })
    

