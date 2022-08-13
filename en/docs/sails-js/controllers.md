---
title: "Controllers"
slug: "controllers"
draft: false
images: []
weight: 9991
type: docs
toc: true
---

Controllers (the **C** in **MVC**) are the principal objects in your Sails application that are responsible for responding to requests from a web browser, mobile application or any other system capable of communicating with a server. They often act as a middleman between your models and views. For many applications, the controllers will contain the bulk of your project’s business logic.

## ES2015 Syntax
```javascript
'use strict';

// This is an example of a /api/controllers/HomeController.js
module.exports = {
  // This is the index action and the route is mapped via /config/routes.js
  index(req, res) {
    // Return a view without view model data
    // This typically will return the view defined at /views/home/index.<view engine extension>
    return res.view();
  },
  foo(req, res) {
    // Return the 'foo' view with a view model that has a `bar` variable set to the query string variable `foobar`
    return res.view({
      bar: req.param('foobar'),
    });
  },
};
```

## Using ES2015 generators with co.js
```javascript
'use strict';

const co = require('co');

module.exports = {
  // This is the index action and the route is mapped via /config/routes.js
  index(req, res) {
    co(function* index() {
      // Return a view without view model data
      // This typically will return the view defined at /views/home/index.<view engine extension>
      return res.view();
    }).catch(res.negotiate); // Catch any thrown errors and pass the error to the `negotiate` policy.
  },
  foo(req, res) {
    co(function* foo() {
      // Get an array of `FooBar` items from the database
      const items = yield FooBar.find();

      // Return the 'foo' view with a view model containing the array of `FooBar` items
      return res.view({
        items,
      });
    }).catch(res.negotiate); // Catch any thrown errors and pass the error to the `negotiate` policy.
  },
};
```

