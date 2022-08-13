---
title: "Blueprint API"
slug: "blueprint-api"
draft: false
images: []
weight: 9873
type: docs
toc: true
---

### How does Blueprint API works?
When sails initially starts using `sails lift`, sails looks to see if you have 
any controller defined. In our example, we have one controller, the User 
controller. Sails then provides access to blueprint actions for this user 
controller as if we built them in the controller ourselves. Sails also 
automatically creates blueprint routes at the time of lifting the server. So even
if no routes is defined in `/config/routes.js` and no action is defined in
`/api/controllers/UserController.js` explicitly, after lifting the server all 
these routes and actions are available to use.

## Blueprint Routes
When you run `sails lift` with blueprints enabled, the framework inspects your 
controllers, models, and configuration in order to bind certain routes automatically. These implicit blueprint routes allow your app to respond 
to certain requests without you having to bind those routes manually in your 
`config/routes.js` file. By default, the blueprint routes point to their 
corresponding blueprint *actions*, any of which 
can be overridden with custom code.

There are three types of blueprint routes in Sails:

+ **RESTful routes**, where the path is always `/:model/:id?`. When the `User` 
  model and controller is defined, blueprint binds RESTful routes implicitly in 
  following way -
  ```
  'GET /user/:id?': {
    controller: 'User',
    action: 'find'
  },
  'POST /user': {
    controller: 'User',
    action: 'create'
  },
  'PUT /user/:id?': {
    controller: 'User',
    action: 'update'
  },
  'DELETE /user/:id?': {
    controller: 'User',
    action: 'destroy'
  }
  ```
  These routes use the HTTP verb to determine the action to take even if the 
  route is same. So, a `POST` request to `/user` will create a new user, a
  `PUT` request to `/user/123` will update the user with primary key 123 and 
  a `DELETE` request to `/user/123` will delete the user whose primary key is 123. 
  In a production environment, RESTful routes should generally be protected by 
  policies to avoid unauthorized access.


+ **Shortcut routes**, where the action to take is encoded in the path. For our 
  User model and controller Sails binds following four shortcut routes implicitly.
  ```
  'GET /user/find/:id?': {
    controller: 'User',
    action: 'find'
  },
  'GET /user/create/:id?': {
    controller: 'User',
    action: 'create'
  },
  'GET /user/update/:id?': {
    controller: 'User',
    action: 'update'
  },
  'GET /user/destroy/:id?': {
    controller: 'User',
    action: 'destroy'
  }
  ```
  As example, the `/user/create?name=joe` shortcut creates a new user, while 
  `/user/update/1?name=mike` updates the name field of user #1. Note that these 
  routes only respond to `GET` requests. Shortcut routes are very handy for 
  development, but generally should be disabled in a production environment. It's
  not designed to be used in production.

+ **Action routes**, which automatically create routes for your custom controller 
actions. For example, let `query` be a custom action defined in User controller.
Then following routes would be implicitly available to sails -
  ```
  'GET /user/query/:id?': {
    controller: 'User',
    action: 'query'
  },
  'POST /user/query/:id?': {
    controller: 'User',
    action: 'query'
  },
  'PUT /user/query/:id?': {
    controller: 'User',
    action: 'query'
  },
  'DELETE /user/query/:id?': {
    controller: 'User',
    action: 'query'
  }
  ```
  If request is made in `/user/query/:id?` route then independent on the HTTP 
  verb the action would be same. Unlike RESTful and shortcut routes, action 
  routes do *not* require that a controller has a corresponding model file. Which
  means, if you define a controller in `/api/controllers/FooController.js` file 
  but no model in `/api/models/Foo.js` file, there would be no RESTful or 
  shortcut route with `/foo` but there will still be action routes available to 
  use.

# Order of routes matching
When a request comes, 
sails will first match the route against explicitly defined routes. If it matches
then no further matching is done and corresponding action is executed. But if it
doesn't match then the route is matched firstly against blueprint action routes,
if doesn't match then against rest routes and if it doesn't match either then
shortcut routes. So if your `/config/routes.js` file has some entry like the 
following-
```
  '/user/query/:id?': {
    controller: 'User',
    action: 'find'
  }
```
Then you can't expect `query` action routes to work. Because the same route as
`query` action route would be matched against the explicitly define routes and 
`find` action of User controller would be executed.

## Blueprint Actions
Blueprint actions (not to be confused with blueprint action *routes*) are 
generic actions designed to work with any of your controllers that have a model 
of the same name (e.g. `ParrotController` would need a `Parrot` model).  Think of 
them as the default behavior for your application. For instance, if you have a 
`User.js` model and an empty `UserController.js` controller, `find`, `create`, 
`update`, `destroy`, `populate`, `add` and `remove` actions exist implicitly, 
without you having to write them.

By default, the blueprint RESTful routes and shortcut routes are bound to their 
corresponding blueprint actions.  However, any blueprint action can be overridden 
for a particular controller by creating a custom action in that controller file 
(e.g. `ParrotController.find`).  Alternatively, you can override the blueprint 
action _everywhere in your app_ by creating your own custom blueprint action.

Sails ships with the following blueprint actions:

+ find
+ findOne
+ create
+ update
+ destroy
+ populate
+ add
+ remove

## Disabling Blueprint Routes
+ **Global basis**:
  Blueprint API configuration is defined in `/config/blueprint.js` file. You can
  enable or disable all three types of blueprint routes for all controllers from
  there. As example, if you want to disable blueprint shortcut routes for all of
  your controllers but want to keep both action and rest routes enabled, then 
  your `/config/blueprint.js` should be like this -
  ```
  module.exports.blueprints = {
    action: true,
    rest: true,
    shortcut: false
  }
  ```

+ **On per-controller basis**:
  You may also override any of the settings from `/config/blueprints.js` on a 
  per-controller basis by defining a '_config' key in your controller defintion, 
  and assigning it a configuration object with overrides for the settings in this 
  file. As example if you want to have shortcut routes enabled only for your user 
  controller but not for any more controllers then with the above blueprint 
  configuration you have to have following key value pair in user controller.
  ```
  module.exports = {
    _config: {
      actions: true,
      shortcuts: true,
      rest: true
    }
  }

  ```

