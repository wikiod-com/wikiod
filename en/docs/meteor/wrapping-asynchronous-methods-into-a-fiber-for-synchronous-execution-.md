---
title: "Wrapping asynchronous methods into a Fiber for synchronous execution."
slug: "wrapping-asynchronous-methods-into-a-fiber-for-synchronous-execution"
draft: false
images: []
weight: 9979
type: docs
toc: true
---

## Syntax
 1. Meteor.wrapAsync(func, [context])

## Parameters
| Parameters | Details |
| ------ | ------ |
| func: Function| An asynchronous/synchronous function to be wrapped in a Fiber that takes a callback w/ parameters `(error, result)`.   |
| context: Any (optional) | A data context in which the function gets executed upon.|

An asynchronously wrapped function may still be ran asynchronously if a callback with parameters `(error, result) => {}` is given as a parameter to the wrapped function.

The incorporation of `Meteor.wrapAsync` allows for code ridden with callbacks to be simplified given that callbacks can now be neglected in compensation for making the call block its present `Fiber`.

To understand how Fibers work, read here: https://www.npmjs.com/package/fibers. 

## Synchronously executing asynchronous NPM methods w/ callbacks.
This example wraps the asynchronous method `oauth2.client.getToken(callback)` from the package NPM package `simple-oauth2`into a Fiber so that the method may be called synchronously.

    const oauth2 = require('simple-oauth2')(credentials);
    
    const credentials = {
        clientID: '#####',
        clientSecret: '#####',
        site: "API Endpoint Here."
    };
    
    Meteor.startup(() => {
        let token = Meteor.wrapAsync(oauth2.client.getToken)({});
        if (token) {
            let headers = {
                'Content-Type': "application/json",
                'Authorization': `Bearer ${token.access_token}`
            }
    
            // Make use of requested OAuth2 Token Here (Meteor HTTP.get).
        }
    });

