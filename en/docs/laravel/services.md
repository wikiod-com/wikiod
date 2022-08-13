---
title: "Services"
slug: "services"
draft: false
images: []
weight: 9925
type: docs
toc: true
---

## Binding a Singleton to the Service Container
We can bind a class as a Singleton:

<!-- language: lang-php -->

    public function register()
    {
        App::singleton('my-database', function()
        {
            return new Database();
        });
    }

This way, the first time an instance of  `'my-database' ` will be requested to the service container, a new instance will be created. All the successive requests of this class will get back the first created instance:

<!-- language: lang-php -->

    //a new instance of Database is created 
    $db = App::make('my-database'); 

    //the same instance created before is returned
    $anotherDb = App::make('my-database');



## Binding an Interface To Implementation
In a Service Provider `register` method we can bind an interface to an implementation:  

<!-- language: php -->

    public function register()
    {  
        App::bind( UserRepositoryInterface::class, EloquentUserRepository::class );        
    }

From now on, everytime the app will need an instance of `UserRepositoryInterface`, Laravel will auto inject a new instance of `EloquentUserRepository` :

<!-- language: php -->

    //this will get back an instance of EloquentUserRepository 
    $repo = App::make( UserRepositoryInterface:class );




## Using the Service Container as a Dependency Injection Container
We can use the Service Container as a Dependency Injection Container by binding the creation process of objects with their dependencies in one point of the application

Let's suppose that the creation of a `PdfCreator` needs two objects as dependencies; every time we need to build an instance of `PdfCreator`, we should pass these dependencies to che constructor. By using the Service Container as DIC, we define the creation of `PdfCreator` in the binding definition, taking the required dependency directly from the Service Container:  

<!-- language: php -->

    App:bind('pdf-creator', function($app) {
    
        // Get the needed dependencies from the service container.
        $pdfRender = $app->make('pdf-render');
        $templateManager = $app->make('template-manager');
    
        // Create the instance passing the needed dependencies.
        return new PdfCreator( $pdfRender, $templateManager );    
    });

Then, in every point of our app, to get a new `PdfCreator`, we can simply do:

<!-- language: php -->

    $pdfCreator = App::make('pdf-creator');

And the Service container will create a new instance, along with the needed dependencies for us.



## Binding an Instance
We can use the Service Container as a Registry by binding an instance of an object in it and get it back when we'll need it:

<!-- language: lang-php -->

    // Create an instance.
    $john = new User('John');
    
    // Bind it to the service container.
    App::instance('the-user', $john);
    
    // ...somewhere and/or in another class...
    
    // Get back the instance
    $john = App::make('the-user'); 




## Introduction
The **Service Container** is the main Application object. It can be used as a Dependency Injection Container, and a Registry for the application by defining bindings in the Service Providers

**Service Providers** are classes where we define the way our service classes will be created through the application, bootstrap their configuration, and bind interfaces to implementations

**Services** are classes that wrap one or more logic correlated tasks together


