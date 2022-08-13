---
title: "Route Model Binding"
slug: "route-model-binding"
draft: false
images: []
weight: 9981
type: docs
toc: true
---

## Implicit Binding

Laravel automatically resolves Eloquent models defined in routes or controller actions whose variable names match a route segment name. For example:

    Route::get('api/users/{user}', function (App\User $user) {
        return $user->email; 
    });

In this example, since the Eloquent $user variable defined on the route matches the {user} segment in the route's URI, Laravel will automatically inject the model instance that has an ID matching the corresponding value from the request URI. If a matching model instance is not found in the database, a 404 HTTP response will automatically be generated.

If the model's table name is composed from multiple words, to make the implicit model binding working the input variable should be all lowercase;  
For example, if the user can do some kind of *action*, and we want to access this action, the route will be:

    Route::get('api/useractions/{useraction}', function (App\UserAction $useraction) {
        return $useraction->description; 
    });

## Explicit Binding
To register an explicit binding, use the router's model method to specify the class for a given parameter. You should define your explicit model bindings in the boot method of the  RouteServiceProvider class

    public function boot()
    {
       parent::boot();

       Route::model('user', App\User::class);
    }

Next, we can define a route that contains {user} parameter.

    $router->get('profile/{user}', function(App\User $user) {
      
    });

Since we have bound all `{user}` parameters to the `App\User` model, a User instance will be injected into the route. So, for example, a request to `profile/1` will inject the User instance from the database which has an **ID** of **1**.

If a matching model instance is not found in the database, a **404 HTTP** response will be automatically generated.

