---
title: "Writing Express Middleware"
slug: "writing-express-middleware"
draft: false
images: []
weight: 9988
type: docs
toc: true
---

## Syntax
 1. Specify the instance of express you want to use. This is commonly *app*.
 2. Define the HTTP method for which the function applies. In the example, this is *get*.
 3. Define the path to which the function applies. In the example, this is *'/'*.
 4. Define as a function with the *function* keyword.
 5. Add the required parameters: req, res, next. (See note in remarks section)
 6. Put some code in the function to do whatever you want

## Parameters
| Parameter | Details |
| --------- | ------- |
| req       | The request object. |
| res       | The response object.|
| next      | The next() middleware call. |

A middleware function is a function with access to the request object (*req*), the response object (*res*), and the *next()* middleware function in the application's request-response cycle. The *next()* middleware function is commonly denoted by a variable named next.

Middleware functions are designed to perform the following tasks:

 - Execute any code.
 - Make changes to the request and response objects. (See the requestTime example)
 - End the request-response cycle. 
 - Call the next middleware in the stack. (By calling the *next()* middleware)

Note: It doesn't have to be named next. But if you use something else no one will know what you mean and you will be fired. And your code won't work. So, just name it next. This rule applies to the request and response object. Some people will use request and response instead of req and res, respectively. That's fine. It wastes keystrokes, but it's fine.

## CORS Middleware
This example demonstrates how a cross origin http request can be handled using a middleware. 


**CORS Background**

CORS is an access control method adopted by all major browsers to avert Cross Scripting Vulnerabilities inherent by them. In general browser security, scripts should maintain that all XHR requests has to be made only to the source the same scripts are served from. If an XHR request is made outside the domain the scripts are belonging to, the response will be rejected.

However if the browser supports CORS, it would make an exception to this rule if appropriate headers in the response indicate that the domain which the request is originated from is allowed. The following header indicates that any domain is allowed:

    Access-Control-Allow-Origin: *


**Example**

Following example shows how Express middleware can include these headers in it's response.


    app.use(function(request, response, next){
    
        response.header('Access-Control-Allow-Origin', '*');
        response.header('Access-Control-Allow-Methods', 'GET,PUT,POST,DELETE,OPTIONS');
        response.header('Access-Control-Allow-Headers', 'Content-Type, Authorization, Content-Length, X-Requested-With');
    
       //Handle Preflight 
       if (reqest.method === 'OPTIONS') {
          response.status(200).send();        
       }
       else {
          next();
       }
    
    });

**Handling Preflight**

The latter part of the above example handles Preflight. Preflight is a special OPTIONS request the browser send to test CORS if the request contain custom headers.

**Useful References**

[MDN - CORS][1]
[Http Tutorial][2]


  [1]: https://developer.mozilla.org/en-US/docs/Web/HTTP/Access_control_CORS
  [2]: https://www.tutorialspoint.com/http/

## Logger Middleware
If you are new to middleware in Express check out the Overview in the Remarks section.

First, we are going to setup a simple Hello World app that will be referenced and added to during the examples.

    var express = require('express');
    var app = express();

    app.get('/', function(req, res) {
        res.send('Hello World!');
    });

    app.listen(3000);
Here is a simple middleware function that will log "LOGGED" when it is called.

    var myLogger = function (req,res,next) {
        console.log('LOGGED');
        next();
    };
*Calling **next()** invokes the next middleware function in the app.*

To load the function call app.use() and specify the function you wish to call. This is done in the following code block that is an extension of the Hello World block.

    var express = require('express');
    var app = express();

    var myLogger = function (req, res, next) {
        console.log('LOGGED');
        next();
    };

    app.use(myLogger);

    app.get('/', function(req, res) {
        res.send('Hello World!');
    });

    app.listen(3000);

Now every time the app receives a request it prints the message "LOGGED" to the terminal. So, how do we add more specific conditions to when middleware is called? Look at the next example and see. 

## requestTime Middleware
Let's create middleware that adds a property called requestTime to the request object.

    var requestTime = function (req, res, next) {
        req.requestTime = Date.now();
        next();
    };

Now let's modify the logging function from the previous example to utilize the requestTime middleware.

    myLogger = function (req, res, next, requestTime) {
        console.log('LOGGED at ' + requestTime);
        next();
    };

Let's add the middleware to our app:

    var express = require('express');
    var app = express();

    myLogger = function (req, res, next) {
        console.log('LOGGED at ' + req.requestTime);
        next();
    };

    var requestTime = function(req, res, next) {
        req.requestTime = Date.now();
        next();
    };

    app.use(requestTime);

    app.use(myLogger);

    app.get('/', function(req, res) {
        res.send('Hello World!');
    });

    app.listen(3000);

Now the app will log the time at which the request was made. This covers the basics of writing and using Express middleware. For more information see [Using Express Middleware][1].

!!!TODO: Create Using Express Middleware Section!!!


  [1]: http://stackoverflow.com

