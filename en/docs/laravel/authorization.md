---
title: "Authorization"
slug: "authorization"
draft: false
images: []
weight: 9979
type: docs
toc: true
---

Laravel provides a simple way to authorise user actions on specific resources. With Authorization, you can selectively allow users access to certain resources while denying access to others. Laravel provides a simple API for managing user authorizations by using `Gates` and `Policies`. `Gates` provide a simple closure based approach to authorisation using the `AuthServiceProvider` while `Policies` allow you to organise authorisation logic around models using classes.

## Using Gates
`Gates` are closures that determine if a user is allowed to perform a certain action on a resource. `Gates` are typically defined in the boot method of `AuthServiceProvider` and succinctly named to reflect what it's doing. An example of a gate that allows only premium users to view some content will look like this:

    Gate::define('view-content', function ($user, $content){
        return $user->isSubscribedTo($content->id);
    });

A `Gate` always receives a user instance as the first argument, you don't need to pass it when using the gate, and may optionally receive additional arguments such as the eloquent model in concern.



## Authorizing Actions with Gates
To use the example above on a blade template to hide content from the user, you would typically do something like this:

    @can('view-content', $content)
     <! -- content here -->
    @endcan


To completely prevent navigation to the content, you can do the following in your controller:

    if(Gate::allows('view-content', $content)){
        /* user can view the content */
    }
    
            OR

    if(Gate::denies('view-content', $content)){
        /* user cannot view content */
    }
Note: You are not required to pass the currently authenticated user to these method, Laravel takes care of that for you.

## Policies
Policies are classes that help you organise authorisation logic around a model resource. Using our previous example, we might have a `ContentPolicy` that manages user access to the `Content` model.

To make `ContentPolicy`, laravel provides an artisan command. Simply run 

```php artisan make:policy ContentPolicy```

This will make an empty policy class and place in `app/Policies` folder. If the folder does not exist, Laravel will create it and place the class inside.

Once created, policies need to be registered to help Laravel know which policies to use when authorising actions on models. Laravel's `AuthServiceProvider`, which comes with all fresh Laravel installations, has a policies property which maps your eloquent models to their authorisation policies. All you need to do add the mapping to the array.

    protected $policies = [
        Content::class => ContentPolicy::class,
     ];



## Writing Policies
Writing `Policies` follows much the same pattern as writing `Gates`. The content permission gate can be rewritten as a Policy like this:

    function view($user, $content)
    {
        return $user->isSubscribedTo($content->id);
    }

Policies can contain more methods as needed to take care of all authorisation cases for a model.

## Authorizing Actions with Policies
**Via The User model**

The Laravel User model contains two methods that help with authorisations using `Policies`; `can` and `can't`. These two can be used to determine if a user has authorisation or not on a model respectively.

To check if a user can view a content or not, you can do the following:

    if($user->can('view', $content)){
        /* user can view content */
    }
    
            OR

    if($user->cant('view', $content)){
        /* user cannot view content */
    }

**Via Middleware**

    Route::get('/contents/{id}, function(Content $content){
        /* user can view content */
    })->middleware('can:view,content');

**Via Controllers**

Laravel provides a helper method, called `authorize` that takes the name of the policy and the associated model as arguments, and either authorizes the action based on your authorisation logic or denies the action and throws an `AuthorizationException` which the Laravel Exception handler converts to a `403 HTTP response`.

    pubic function show($id)
    {
        $content = Content::find($id);

        $this->authorize('view', $content);
            
        /* user can view content */
    }

