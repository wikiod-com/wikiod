---
title: "Middleware"
slug: "middleware"
draft: false
images: []
weight: 9957
type: docs
toc: true
---

Middleware are classes, that can be assigned to one or more route, and are used to make actions in the early or final phases of the request cycle. We can think of them as a series of layers an HTTP request has to pass through while it's executed

A "Before" middleware will executes before the controller action code; while a "After" middleware executes after the request is handled by the application


## Defining a Middleware
To define a new middleware we have to create the middleware class:

<!-- language: php -->

    class AuthenticationMiddleware
    {
        //this method will execute when the middleware will be triggered
        public function handle ( $request, Closure $next )
        {
            if ( ! Auth::user() )
            {
                return redirect('login');
            }
    
            return $next($request);
        }
    }

Then we have to register the middleware: if the middleware should be bind to all the routes of the application, we should add it to the middleware property of `app/Http/Kernel.php`:

<!-- language: php -->

    protected $middleware = [
            \Illuminate\Foundation\Http\Middleware\CheckForMaintenanceMode::class,
            \App\Http\Middleware\AuthenticationMiddleware::class
    ];

while if we only want to associate the middleware to some of the routes, we can add it to `$routeMiddleware`

<!-- language: php -->

    //register the middleware as a 'route middleware' giving it the name of 'custom_auth'
    protected $routeMiddleware = [
        'custom_auth' => \App\Http\Middleware\AuthenticationMiddleware::class
    ];

and then bind it to the single routes like this:

<!-- language: php -->

    //bind the middleware to the admin_page route, so that it will be executed for that route
    Route::get('admin_page', 'AdminController@index')->middleware('custom_auth');



## Before vs. After Middleware
An example of "before" middleware would be as follows:

    <?php
    
    namespace App\Http\Middleware;
    
    use Closure;
    
    class BeforeMiddleware
    {
        public function handle($request, Closure $next)
        {
            // Perform action
    
            return $next($request);
        }
    }

while "after" middleware would look like this:

    <?php
    
    namespace App\Http\Middleware;
    
    use Closure;
    
    class AfterMiddleware
    {
        public function handle($request, Closure $next)
        {
            $response = $next($request);
    
            // Perform action
    
            return $response;
        }
    }
The key difference is in how the `$request` parameter is handled. If actions are performed before `$next($request)` that will happen before the controller code is executed while calling `$next($request)` first will lead to the actions being performed after the controller code is executed.

## Route Middleware
Any middleware registered as `routeMiddleware` in `app/Http/Kernel.php` can be assigned to a route.

There are a few different ways to assign middleware, but they all do the same.

    Route::get('/admin', 'AdminController@index')->middleware('auth', 'admin');
    Route::get('admin/profile', ['using' => 'AdminController@index', 'middleware' => 'auth']);
    Route::get('admin/profile', ['using' => 'AdminController@index', 'middleware' => ['auth', 'admin']);

In all the examples above, you can also pass fully qualified class names as middleware, regardless if it's been registered as a route middleware.

    use App\Http\Middleware\CheckAdmin;
    Route::get('/admin', 'AdminController@index')->middleware(CheckAdmin::class);

