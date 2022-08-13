---
title: "Requests"
slug: "requests"
draft: false
images: []
weight: 9986
type: docs
toc: true
---

## Obtain an Instance of HTTP Request
To obtain an instance of an HTTP Request, class `Illuminate\Http\Request` need to be type hint either in the constructor or the method of the controller. 

Example code: 

   ```php
    <?php

namespace App\Http\Controllers;

/* Here how we illuminate the request class in controller */
use Illuminate\Http\Request; 

use Illuminate\Routing\Controller;

class PostController extends Controller
{
    /**
     * Store a new post.
     *
     * @param  Request  $request
     * @return Response
     */
    public function store(Request $request)
    {
        $name = $request->input('post_title');

        /*
        * so typecasting Request class in our method like above avails the
        * HTTP GET/POST/PUT etc method params in the controller to use and 
        * manipulate
        */
    }
}

## Request Instance with other Parameters from routes in controller method
Sometimes we need to accept route params as well as access the HTTP Request params. We can still type hint the Requests class in laravel controller and achieve that as explained below


E.g. We have a route that update a certain post like this (passing post id i route )

    Route::put('post/{id}', 'PostController@update');

Also since user have edited other edit form fields, so that will be available in HTTP Request

Here is how to access both in our method
```php

    public function update(Request $request,$id){
        //This way we have $id param from route and $request as an HTTP Request object
        
    }



