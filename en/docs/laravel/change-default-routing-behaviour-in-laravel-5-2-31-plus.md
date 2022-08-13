---
title: "Change default routing behaviour in Laravel 5.2.31 +"
slug: "change-default-routing-behaviour-in-laravel-5231-+"
draft: false
images: []
weight: 9962
type: docs
toc: true
---

## Syntax
 - public function map(Router $router) // Define the routes for the application.
 - protected function mapWebRoutes(Router $router) // Define the "web" routes for the application.

## Parameters
Parameter | Header 
-----     | ----- 
Router $router | \Illuminate\Routing\Router  $router 

Middleware means that every call to a route will go through the middleware before actually hitting your route specific code. In Laravel the web middleware is used to ensure session handling or the csrf token check for example.

There are other middlewares like auth or api by default. You can also easily create your own middleware.



## Adding api-routes with other middleware and keep default web middleware
Since Laravel version 5.2.31 the web middleware is applied by default within the RouteServiceProvider (https://github.com/laravel/laravel/commit/5c30c98db96459b4cc878d085490e4677b0b67ed) 

In app/Providers/RouteServiceProvider.php you will find the following functions which apply the middleware on every route within your app/Http/routes.php

    public function map(Router $router)
    {
        $this->mapWebRoutes($router);
    }

    // ...

    protected function mapWebRoutes(Router $router)
    {
        $router->group([
            'namespace' => $this->namespace, 'middleware' => 'web',
        ], function ($router) {
            require app_path('Http/routes.php');
        });
    }

As you can see the **middleware** web is applied. You could change this here. However, you can also easily add another entry to be able to put your api routes for example into another file (e.g. routes-api.php)

    public function map(Router $router)
    {
        $this->mapWebRoutes($router);
        $this->mapApiRoutes($router);
    }

    protected function mapWebRoutes(Router $router)
    {
        $router->group([
            'namespace' => $this->namespace, 'middleware' => 'web',
        ], function ($router) {
            require app_path('Http/routes.php');
        });
    }

    protected function mapApiRoutes(Router $router) 
    {
        $router->group([
            'namespace' => $this->namespace, 'middleware' => 'api',
        ], function ($router) {
            require app_path('Http/routes-api.php');
        });
    }

With this you can easily seperate you api routes from your application routes without the messy group wrapper within your routes.php

