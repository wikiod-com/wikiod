---
title: "Helpers"
slug: "helpers"
draft: false
images: []
weight: 9980
type: docs
toc: true
---

Laravel helpers are the globally accessible functions defined by the framework. It can be directly called and independently used anywhere within the application without needing to instantiating an object or importing class.



There are helpers for manipulating *Arrays*, *Paths*, *Strings*, *URLs*, etc


## Array methods
**array_add()**

This method is used to add new key value pairs to an array. 

    $array = ['username' => 'testuser'];

    $array = array_add($array, 'age', 18);

result

    ['username' => 'testuser', 'age' => 18]

## String methods
**camel_case()**

This method changes a string to camel case

    camel_case('hello_world');

result

    HelloWorld

## Path mehods
Path methods helps easy access to application related paths easily from anywhere.

**public_path()**

This method returns the fully qualified public path of the application. which is the public directory. 
    
    $path = public_path();


## Urls
**url()**

The url function generates a fully qualified URL to the given path.

if your site is `hello.com`

    echo url('my/dashboard');

would return

    hello.com/my/dashboard

if nothing is passed to the url method it would return an instance of `Illuminate\Routing\UrlGenerator`, and it could be used like this

would return current url

    echo url()->current();

would return full url

    echo url()->full();

would return previous url

    echo url()->previous();



