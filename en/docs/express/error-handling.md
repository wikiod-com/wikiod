---
title: "Error handling"
slug: "error-handling"
draft: false
images: []
weight: 9944
type: docs
toc: true
---

## Syntax
 - `app.use(function(err, req, res, next) {})` // Basic middleware

## Parameters
| Name | Description |
| --- | --- |
| `err` | Object with error information |
| `req` | HTTP request object |
| `res` | HTTP response object |
| `next` | function used to start next middleware execution |

## Basic sample
Unlike other middleware functions error-handling middleware functions have four arguments instead of three: `(err, req, res, next)`. 

Sample:

    app.use(function(err, req, res, next) {
      console.error(err.stack);
      res.status(500).send('Error found!');
    });





