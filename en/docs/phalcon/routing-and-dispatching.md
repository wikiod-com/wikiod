---
title: "Routing and dispatching"
slug: "routing-and-dispatching"
draft: false
images: []
weight: 9987
type: docs
toc: true
---

## RESTful API Routes for Multi Module Application
    // Define new router group
    $api = new \Phalcon\Mvc\Router\Group([
        'module' => 'api',
    ]);
    $api->setPrefix('/api/v1');
    
    // API routes (Maps to Cotnroller::Action)
    $api->addGet('/users', 'Users::index');
    $api->addGet('/users/search/{query}', 'Users::search');
    $api->addGet('/users/{id:[0-9]+}', 'Users::fetch');
    $api->addPost('/users', 'Users::add');
    $api->addPut('/users/{id:[0-9]+}', 'Users::edit');
    $api->addDelete('/users/{id:[0-9]+}', 'Users::delete'); 
    
    // Add API routes to main router
    $router->mount($api);

Example of creating a user:

    curl -i -X POST -d 
        '{"name": "John Snow", "title": "King of the North"}' 
        http://example.com/api/v1/users

## Dynamically set module routes
```
$router = new \Phalcon\Mvc\Router(false);
$router->removeExtraSlashes(true);
$request = new \Phalcon\Http\Request();
$action = strtolower($request->getMethod()); // get, post, etc.
$modules = ['calendar', 'main', 'user']; // names of the modules you create

// you can define other static routes here

foreach ($modules as $module) {
    // must match what you register with the Loader service
    $namespace = 'App\\' . ucfirst($module) . '\Controllers'; 
    
    // make a group to avoid setting namespace and module for every route definition
    $moduleGroup = new \Phalcon\Mvc\Router\Group([
        'namespace' => $namespace,
        'module' => $module
    ]);
    
    // this will match a route like /calendar/index/save
    $moduleGroup->add("/{$module}/:controller/:action", [
        'controller' => 1,
        'action' => 2
    ]);
    
    // setting a prefix will apply it to all routes below
    $moduleGroup->setPrefix('/api');
    
    // this will match a route like /api/calendar/index/save
    $moduleGroup->add("/{$module}/([a-zA-Z_]+)/:action", [
        'controller' => 1,
        'action' => 2
    ]);

    // this will match a route like /api/calendar/123
    $moduleGroup->add("/{$module}/:int", [
        'moduleId' => 1,
        'controller' => 'index',
        'action' => $action // defined at the top of example
    ]);

    $router->mount($moduleGroup);
}

// you can define other static routes here

return $router;

