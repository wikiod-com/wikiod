---
title: "Explain Routing in Express"
slug: "explain-routing-in-express"
draft: false
images: []
weight: 9989
type: docs
toc: true
---

## Chainable route handlers for a route path by using app.route
    var express   = require('express');
    var app       = express();
    var router    = express.Router();
    
    app.route('/user')
      .get(function (req, res) {
        res.send('Get a random user')
      })
      .post(function (req, res) {
        res.send('Add a user')
      })
      .put(function (req, res) {
        res.send('Update the user details')
      })
      .delete(function (req, res) {
        res.send('Delete a user')
      });

## Express Router
Express router allows you to create multiple "mini apps" so you can namespace your api, public, auth and other routes into separate routing systems. 
```
var express   = require('express');
var app       = express();
var router    = express.Router();

router.get('/', function(req, res){
   res.send('Get request received');
});

router.post('/', function(req, res){
   res.send('Post requestreceived');
});

app.use('/', router);

app.listen(8080);
```

