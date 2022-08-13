---
title: "Getting started with hapijs"
slug: "getting-started-with-hapijs"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Hello world
Create a `server.js` file with the following contents:
----------------------------------------------------

<!-- language: lang-js -->
    'use strict';

    const Hapi = require('hapi');

    // Create a server instance
    const server = new Hapi.Server();

    // Specify connections (server available on http://localhost:8000)
    server.connection({ 
        port: 8000 
    });

    // Add a route
    server.route({
        method: 'GET',
        path:'/hello', 
        handler: function (request, reply) {
            return reply('hello world');
        }
    });

    // Start the server
    server.start((err) => {
        if (err) {
            throw err;
        }

        console.log('Server running at:', server.info.uri);
    });

Start Hapi.js Server
----------------------

Run `node server.js` and open http://localhost:8000/hello in your browser.

## Passing Parameters to a route
Parameters can be specified in `path` property of route configuration

<!-- language: lang-js -->
    'use strict';

    const Hapi = require('hapi');

    // Create a server with a host and port
    const server = new Hapi.Server();

    server.connection({ 
        host: 'localhost', 
        port: 8000 
    });

    // Add a route path with url param
    server.route({
        method: 'GET',
        path:'/hello/{name}', 
        handler: function (request, reply) {
            // Passed parameter is accessible via "request.params" 
            return reply(`Hello ${request.params.name}`);
        }
    });

    // Start the server
    server.start((err) => {
        if (err) {
            throw err;
        }
        console.log('Server running at:', server.info.uri);
    });


## Validation
<!-- language: lang-js -->
    'use strict';
    
    const Hapi = require('hapi');
    const Joi = require('joi');
    
    // Create a server with a host and port
    const server = new Hapi.Server();
    
    server.connection({ 
        host: 'localhost', 
        port: 8000 
    });
    
    /**
     * Add a route path with url param
     */
    server.route({
        method: 'GET',
        path:'/hello/{name}', 
        handler: function (request, reply) {
            // Passed parameter is accessible via "request.params" 
            return reply(`Hello ${request.params.name}`);
        },
        config: {
            // Validate the {name} url param
            validate: {
                params: Joi.string().required()
            }
        }
    });
    
    // Start the server
    server.start((err) => {
        if (err) {
            throw err;
        }
        console.log('Server running at:', server.info.uri);
    });

