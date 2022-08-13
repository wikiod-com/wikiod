---
title: "Routing"
slug: "routing"
draft: false
images: []
weight: 9955
type: docs
toc: true
---

## Routing Hello World
The main app file loads the routes file where routes are defined.

app.js

    var express = require('express');
    var app = express();
    
    app.use('/', require('./routes'));
    
    app.listen('3000');

routes.js

    var router = require('express').Router();
    
    router.get('/', function(req, res) {
        res.send('Hello World!');
    });
    
    module.exports = router;

## Multiple Routes
The main app file loads any routes files in which you would like to define routes.
To do so we need the following directory structure:
app.js
routes/index.js
routes/users.js

app.js

    var express = require('express');
    var app = express();

    app.use('/', require('./routes/index'));
    app.use('/users', require('./routes/users'))

    app.listen('3000');

routes/index.js

    var router = require('express').Router();

    router.get('/', function(req, res) {
        res.send('Index Page');
    });

    router.get('/about', function(req, res) {
        res.send('About Page');
    });

    module.exports = router;

routes/users.js

    var router = require('express').Router();

    router.get('/', function(req, res) {
        res.send('Users Index Page');
    });

    router.get('/list', function(req, res) {
        res.send('Users List Page');
    });

    module.exports = router;


Running `$ node app.js` there should now be pages at the following urls:
- localhost:3000/ - Displays "Index Page"
- localhost:3000/about - Displays "About Page"
- localhost:3000/users -  Displays "Users Index Page"
- localhost:3000/users/list - Displays "Users List Page"


## Routing middleware
Middleware is executed prior to the route execution and can decide whether to execute the router according to the URL.

    var router = require('express').Router();
    
    router.use(function (req, res, next) {
        var weekDay = new Date().getDay();
        if (weekDay === 0) {
            res.send('Web is closed on Sundays!');
        } else {
            next();
        }
    })
    
    router.get('/', function(req, res) {
        res.send('Sunday is closed!');
    });
    
    module.exports = router;

----
Specific middleware can also be sent to each router handler.

    var closedOnSundays = function (req, res, next) {
        var weekDay = new Date().getDay();
        if (weekDay === 0) {
            res.send('Web is closed on Sundays!');
        } else {
            next();
        }
    }
    
    router.get('/', closedOnSundays, function(req, res) {
        res.send('Web is open');
    });
    
    router.get('/open', function(req, res) {
        res.send('Open all days of the week!');
    });

