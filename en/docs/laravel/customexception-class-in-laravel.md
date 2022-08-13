---
title: "CustomException class in Laravel"
slug: "customexception-class-in-laravel"
draft: false
images: []
weight: 9994
type: docs
toc: true
---

PHP Exceptions are thrown when an unprecedented event or error occurs.

 As a rule of thumb, an exception should not be used to control the application logic such as if-statements and should be a subclass of the Exception class.

One main advantage of having all exceptions caught by a single class is that we are able to create custom exception handlers that return different response messages depending on the exception.

## CustomException class in laravel
all errors and exceptions, both custom and default, are handled by the Handler class in app/Exceptions/Handler.php with the help of two methods.

 - report()
 - render()

       public function render($request, Exception $e)
       {
        //check if exception is an instance of ModelNotFoundException.
        if ($e instanceof ModelNotFoundException)
        {
            // ajax 404 json feedback
            if ($request->ajax())
            {
                return response()->json(['error' => 'Not Found'], 404);
            }
            // normal 404 view page feedback
            return response()->view('errors.missing', [], 404);
         }
          return parent::render($request, $e);
       }

then create view related to error in errors folder named 404.blade.php

<!DOCTYPE html>
<html>
<head>
    <title>User not found.</title>
</head>
<body>
    <p>You broke the balance of the internet</p>
</body>
</html>

