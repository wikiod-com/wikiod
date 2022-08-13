---
title: "Error Handling"
slug: "error-handling"
draft: false
images: []
weight: 9979
type: docs
toc: true
---

Remember to set up your application for emailing by ensuring proper configuration of `config/mail.php`

Also check to make sure ENV variables are properly set.

This example is a guide and is minimal. Explore, modify and style the view as you wish. Tweak the code to meet your needs. For example, set the recepient in your *.env* file

## Send Error report email
Exceptions in Laravel are handled by *App\Exceptions\Handler.php*

This file contains two functions by default.
Report & Render. We will only be using the first

<!-- language: lang-php -->

     public function report(Exception $e)

> The report method is used to log exceptions or send them to an external service like BugSnag. By default, the report method simply passes the exception to the base class where the exception is logged. However, you are free to log exceptions however you wish.

Essentially this function just forwards the error and does nothing. Therefore, we can insert business logic to perform operations based on the error. For this example we will be sending an email containing the error information.

<!-- language: lang-php -->

    public function report(Exception $e)
    {
        if ($e instanceof \Exception) {
            // Fetch the error information we would like to 
            // send to the view for emailing
            $error['file']    = $e->getFile();
            $error['code']    = $e->getCode();
            $error['line']    = $e->getLine();
            $error['message'] = $e->getMessage();
            $error['trace']   = $e->getTrace();

            // Only send email reports on production server
            if(ENV('APP_ENV') == "production"){
                #1. Queue email for sending on "exceptions_emails" queue
                #2. Use the emails.exception_notif view shown below
                #3. Pass the error array to the view as variable $e
                Mail::queueOn('exception_emails', 'emails.exception_notif', ["e" => $error], function ($m) {
                    $m->subject("Laravel Error");
                    $m->from(ENV("MAIL_FROM"), ENV("MAIL_NAME"));
                    $m->to("webmaster@laravelapp.com", "Webmaster");
                });

            }
        }

        // Pass the error on to continue processing
        return parent::report($e);
    }

The view for the email ("emails.exception_notif") is below

<!-- language: lang-php -->

    <?php 
    $action = (\Route::getCurrentRoute()) ? \Route::getCurrentRoute()->getActionName() : "n/a";
    $path = (\Route::getCurrentRoute()) ? \Route::getCurrentRoute()->getPath() : "n/a";
    $user = (\Auth::check()) ? \Auth::user()->name : 'no login';
    ?>

    There was an error in your Laravel App<br />
    
    <hr />
    <table border="1" width="100%">
        <tr><th >User:</th><td>{{ $user }}</td></tr>
        <tr><th >Message:</th><td>{{ $e['message'] }}</td></tr>
        <tr><th >Action:</th><td>{{ $action }}</td></tr>
        <tr><th >URI:</th><td>{{ $path }}</td></tr>
        <tr><th >Line:</th><td>{{ $e['line'] }}</td></tr>
        <tr><th >Code:</th><td>{{ $e['code'] }}</td></tr>
    </table>




## Catching application wide ModelNotFoundException
**app\Exceptions\Handler.php**

    public function render($request, Exception $exception)
    {
        if ($exception instanceof ModelNotFoundException) {
            abort(404);
        }
    
        return parent::render($request, $exception);
    }

You can catch / handle any exception that is thrown in Laravel.

