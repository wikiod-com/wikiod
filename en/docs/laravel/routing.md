---
title: "Routing"
slug: "routing"
draft: false
images: []
weight: 9861
type: docs
toc: true
---

## Basic Routing
<!-- language-all: lang-php -->

Routing defines a map between HTTP methods and URIs on one side, and actions on the other.
Routes are normally written in the `app/Http/routes.php` file.

In its simplest form, a route is defined by calling the corresponding HTTP method on the Route facade, passing as parameters a string that matches the URI (relative to the application root), and a callback.

For instance: a route to the root URI of the site that returns a view `home` looks like this:

    Route::get('/', function() {
        return view('home');
    });

A route for a post request which simply echoes the post variables:

    Route::post('submit', function() {
       return Input::all();
    });

    //or

    Route::post('submit', function(\Illuminate\Http\Request $request) {
       return $request->all();
    });

 

Routes pointing to a Controller method
===============================

Instead of defining the callback inline, the route can refer to a controller method in [ControllerClassName@Method] syntax:

    Route::get('login', 'LoginController@index');


A route for multiple verbs
==========================

The `match` method can be used to match an array of HTTP methods for a given route:

    Route::match(['GET', 'POST'], '/', 'LoginController@index');


Also you can use `all` to match any HTTP method for a given route:

    Route::all('login', 'LoginController@index');

Route Groups
============

Routes can be grouped to avoid code repetition.

Let's say all URIs with a prefix of ```/admin``` use a certain middleware called ```admin``` and they all live in the ```App\Http\Controllers\Admin``` namespace.

A clean way of representing this using Route Groups is as follows:

    Route::group([
        'namespace' => 'Admin',
        'middleware' => 'admin',
        'prefix' => 'admin'
        ], function () {
            
            // something.dev/admin
            // 'App\Http\Controllers\Admin\IndexController'
            // Uses admin middleware
            Route::get('/', ['uses' => 'IndexController@index']);    
            
            // something.dev/admin/logs
            // 'App\Http\Controllers\Admin\LogsController'
            // Uses admin middleware
            Route::get('/logs', ['uses' => 'LogsController@index']);    
    });



## Routes are matched in the order they are declared
This is a common gotcha with Laravel routes. Routes are matched in the order that they are declared. The first matching route is the one that is used.

This example will work as expected:

```php
Route::get('/posts/{postId}/comments/{commentId}', 'CommentController@show');
Route::get('/posts/{postId}', 'PostController@show');
```

A get request to `/posts/1/comments/1` will invoke `CommentController@show`. A get request to `/posts/1` will invoke `PostController@show`.

However, this example will not work in the same manner:

```php
Route::get('/posts/{postId}', 'PostController@show');
Route::get('/posts/{postId}/comments/{commentId}', 'CommentController@show');
```
A get request to `/posts/1/comments/1` will invoke `PostController@show`. A get request to `/posts/1` will invoke `PostController@show`.

Because Laravel uses the first matched route, the request to `/posts/1/comments/1` matches `Route::get('/posts/{postId}', 'PostController@show');` and assigns the variable `$postId` to the value `1/comments/1`. This means that `CommentController@show` will never be invoked.

## Named Route
<!-- language-all: lang-php -->

Named routes are used to generate a URL or redirects to a specific route. The advantage of using a named route is, if we change the URI of a route in future, we wouldn't need to change the URL or redirects pointing to that route if we are using a named route. But if the links were generated using the url [ eg. `url('/admin/login')`], then we would have to change everywhere where it is used. 

Named routes are created using `as` array key

    Route::get('login', ['as' => 'loginPage', 'uses' => 'LoginController@index']);

or using method `name`

    Route::get('login', 'LoginController@index')->name('loginPage');

# Generate URL using named route

To generate a url using the route's name

    $url = route('loginPage');
If you are using the route name for redirection

    $redirect = Redirect::route('loginPage');

## Route Parameters
<!-- language-all: lang-php -->

You can use route parameters to get the part of the URI segment. You can define a optional or required route parameter/s while creating a route. Optional parameters have a `?` appended at the end of the parameter name. This name is enclosed in a curly braces `{}`

# Optional Parameter

    Route::get('profile/{id?}', ['as' => 'viewProfile', 'uses' => 'ProfileController@view']);

This route can be accessed by `domain.com/profile/23` where 23 is the id parameter. In this example the `id` is passed as a parameter in `view` method of `ProfileController`.
Since this is a optional parameter accessing `domain.com/profile` works just fine. 

# Required Parameter

    Route::get('profile/{id}', ['as' => 'viewProfile', 'uses' => 'ProfileController@view']);
Note that required parameter's name doesn't have a `?` at the end of the parameter name.

# Accessing the parameter in controller
In your controller, your view method takes a parameter with the **same** name as the one in the `routes.php` and can be used like a normal variable. Laravel takes care of injecting the value:

    public function view($id){
        echo $id;
    }

## Catch all routes
If you want to catch all routes, then you could use a regular expression as shown:

<!-- language: lang-php -->

    Route::any('{catchall}', 'CatchAllController@handle')->where('catchall', '.*');

**Important:** If you have other routes and you don't want for the catch-all to interfere, you should put it in the end. For example:

# Catching all routes except already defined

<!-- language: lang-php -->

    Route::get('login',  'AuthController@login');
    Route::get('logout', 'AuthController@logout');
    Route::get('home',   'HomeController@home');
    
    // The catch-all will match anything except the previous defined routes.
    Route::any('{catchall}', 'CatchAllController@handle')->where('catchall', '.*');



## Case-insensitive routes
Routes in Laravel are case-sensitive. It means that a route like

    Route::get('login', ...);

will match a GET request to `/login` but will not match a GET request to `/Login`.

In order to make your routes case-insensitive, you need to create a new validator class that will match requested URLs against defined routes. The only difference between the new validator and the existing one is that it will append the **i** modifier at the end of regular expression for the compiled route to switch enable case-insensitive matching.

<!-- language-all: lang-php -->
    <?php namespace Some\Namespace;

    use Illuminate\Http\Request;
    use Illuminate\Routing\Route;
    use Illuminate\Routing\Matching\ValidatorInterface;

    class CaseInsensitiveUriValidator implements ValidatorInterface
    {
      public function matches(Route $route, Request $request)
      {
        $path = $request->path() == '/' ? '/' : '/'.$request->path();
        return preg_match(preg_replace('/$/','i', $route->getCompiled()->getRegex()), rawurldecode($path));
      }
    }

In order for Laravel to use your new validator, you need to update the list of matchers that are used to match URL to a route and replace the original UriValidator with yours.

In order to do that, add the following at the top of your routes.php file:

    <?php
    use Illuminate\Routing\Route as IlluminateRoute;
    use Your\Namespace\CaseInsensitiveUriValidator;
    use Illuminate\Routing\Matching\UriValidator;

    $validators = IlluminateRoute::getValidators();
    $validators[] = new CaseInsensitiveUriValidator;
    IlluminateRoute::$validators = array_filter($validators, function($validator) { 
      return get_class($validator) != UriValidator::class;
    });

This will remove the original validator and add yours to the list of validators.

