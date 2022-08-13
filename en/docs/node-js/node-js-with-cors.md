---
title: "Node.js with CORS"
slug: "nodejs-with-cors"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

## Enable CORS in express.js
As node.js is often used to build API, proper CORS setting can be a life saver if you want to be able to request the API from different domains.

In the exemple, we'll set it up for the wider configuration (authorize all request types from any domain.

In your server.js after initializing express:

    // Create express server
    const app = express();

    app.use((req, res, next) => {
        res.header('Access-Control-Allow-Origin', '*');

        // authorized headers for preflight requests
        // https://developer.mozilla.org/en-US/docs/Glossary/preflight_request
        res.header('Access-Control-Allow-Headers', 'Origin, X-Requested-With, Content-Type, Accept');
        next();

        app.options('*', (req, res) => {
            // allowed XHR methods  
            res.header('Access-Control-Allow-Methods', 'GET, PATCH, PUT, POST, DELETE, OPTIONS');
            res.send();
        });
    });

Usually, node is ran behind a proxy on production servers. Therefore the reverse proxy server (such as Apache or Nginx) will be responsible for the CORS config.

To conveniently adapt this scenario, it's possible to only enable node.js CORS when it's in development.

This is easily done by checking `NODE_ENV`:

    const app = express();

    if (process.env.NODE_ENV === 'development') {
        // CORS settings
    }

