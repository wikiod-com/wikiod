---
title: "Routing"
slug: "routing"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

Routes are rules that tell Sails what to do when faced with an incoming request. 

Routes are defined in `config/routes.js`. The order of the routes is significant, as routes are matched top down. This means if you have a specific route that also could be matched by a wildcard route, the specific route should be defined above the wildcard route.

When a request enters your application sails.js grabs all the parameters that came with it and makes them available for you as `params` on the request object. 

----

Properties in the route target object will be passed through to the route handler in the req.options object. The following are reserved properties that can affect the behavior of the route handler:


| Property | Applicable Target Types | Data Type | Details |
| -------- | ----------------------- | --------- | ------- |
| **skipAssets** | all | Boolean | Set to `true` if you _don't_ want the route to match URLs with dots in them (e.g. myImage.jpg). This will keep your routes with wildcard notation from matching URLs of static assets. Useful when creating URL slugs. |
| **skipRegex** | all | Regexp | If skipping every URL containing a dot is too permissive, or you need a route's handler to be skipped based on different criteria entirely, you can use `skipRegex`. This option allows you to specify a regular expression or array of regular expressions to match the request URL against; if any of the matches are successful, the handler is skipped. Note that unlike the syntax for binding a handler with a regular expression, `skipRegex` expects _actual RegExp objects, not strings. |
| **locals** | controller, view, blueprint, response | Dictionary | Sets default local variables to pass to any view that is rendered while handling the request. |
| **cors** | all | Dictionary or Boolean or String | Specifies how to handle requests for this route from a different origin. |
| **populate** | blueprint | Boolean | Indicates whether the results in a "find" or "findOne" blueprint action should have associated model fields populated. Defaults to the value set in `config/blueprints.js`. |
| **skip**, **limit**, **sort**, **where** | blueprint | Dictionary | Set criteria for "find" blueprint. |

## Custom RESTful route definitions
<!-- language: lang-js -->
    module.exports.routes = {
        'GET /foo': 'FooController.index',
        'GET /foo/new': 'FooController.new',
        'POST /foo/create': 'FooController.create',
        'GET /foo/:id/edit': 'FooController.edit',
        'PUT /foo/:id/update': 'FooController.update',
        'GET /foo/:id': 'FooController.show',
        'DELETE /foo/:id': 'FooController.delete',
    };


## Redirect
```javascript
module.exports.routes = {
  '/foo': '/bar',
  'GET /google': 'http://www.google.com'
};
```

## Define custom variable for all views
```javascript
module.exports.routes = {
  // This function will be executed for all http verbs on all urls
  'all /*', function (req, res, next) {
    // Expose the function `fooBar` to all views (via the locals object)
    res.locals.fooBar = function (arg1) {
      return 'foobar' + arg1;
    };
  },
};
```

## Skip assets (urls with dots in them) from wildcard route
```javascript
module.exports.routes = {
  'GET /foo/*': {
     fn: function(req, res) {
       res.send("FOO!");
     },
     skipAssets: true
  },
};
```

## Routes with RegEx
<!-- language: lang-js -->

    module.exports.routes = {
        // sends matching regex (few patterns) as 'page' param
        'r|/foo/([0-9]+)|page': 'FooController.get',
        'r|/foo/(.*)|page': 'FooController.get',
        'r|/foo/(\\w+)|page': 'FooController.get'
    };

## URL slugs
    module.exports.routes = {
      'GET /blog/:year/:month/:day/:posttitle/': 'BlogController.showPost',
      'GET /blog/:year/:month/:day/': 'BlogController.showDayArchive',
      'GET /blog/:year/:month/': 'BlogController.showMonthArchive',
      'GET /blog/:year/': 'BlogController.showYearArchive',
    };

The parameters passed in the URL can then be accessed in the corresponding controller actions using `req.param('year')`, `req.param('month')` etc.

For example, a `GET` request to `/blog/2016/08/` triggers the `BlogController.showMonthArchive` controller action, with `req.param('year')` having the value `2016` and `req.param('month')` having the value `08`.

