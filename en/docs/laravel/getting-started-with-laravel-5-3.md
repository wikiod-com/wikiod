---
title: "Getting started with laravel-5.3"
slug: "getting-started-with-laravel-53"
draft: false
images: []
weight: 9933
type: docs
toc: true
---

This section provides an overview of what laravel-5.3 is, and why a developer might want to use it.

It should also mention any large subjects within laravel-5.3, and link out to the related topics.  Since the Documentation for laravel-5.3 is new, you may need to create initial versions of those related topics.

## Hello World Example (Basic) and with using a view
**The basic example**  
Open `routes/web.php` file and paste the following code in file:

<!-- language: php -->

    Route::get('helloworld', function () {
        return '<h1>Hello World</h1>';
    });

here '**helloworld**' will act as page name you want to access, 

and if you don't want to create blade file and still want to access the page directly 
then you can use laravel routing this way

now type  `localhost/helloworld` in browser address bar and you can access page displaying Hello World.

**The next step.** <br>
So you've learned how to create a very simple Hello World! page by returning a hello world sentence. But we can make it a bit nicer!

Step 1. <br>
We'll start again at our `routes/web.php` file now instead of using the code above we'll use the following code:

<!-- language: php -->

    Route::get('helloworld', function() {
        return view('helloworld');
    });

The return value this time is not just a simple helloworld text but a view. A view in Laravel is simply a new file. This file "helloworld" contains the HTML and maybe later on even some PHP of the Helloworld text.

Step 2. <br>
Now that we've adjusted our route to call on a view we are going to make the view. Laravel works with blade.php files in views. So, in this case, our route is called helloworld. So our view will be called `helloworld.blade.php`

We will be creating the new file in the `resources/views` directory and we will call it helloworld.blade.php

Now we'll open this new file and edit it by creating our Hello World sentence. We can add multiple different ways to get our sentence as in the example below.

<!-- language: php -->

    <html>
        <body>
            <h1> Hello World! </h1>
    
            <?php
                echo "Hello PHP World!";
            ?>
    
        </body>
    </html>

now go to your browser and type your route again like in the basic example: `localhost/helloworld` you'll see your new created view with all of the contents!

## Local Development Server
If you have PHP installed locally and you would like to use PHP's built-in development server to serve your application, you may use the `serve` Artisan command. This command will start a development server at `http://localhost:8000`:

<!-- language: php -->

    php artisan serve

Of course, more robust local development options are available via [Homestead][1] and [Valet][2].

Also it's possible to use a custom port, something like `8080`. You can do this with the `--port` option.

<!-- language: php -->

    php artisan serve --port=8080

If you have a local domain in your hosts file, you can set the hostname. This can be done by the `--host` option.

<!-- language: php -->

    php artisan serve --host=example.dev

You can also run on a custom host and port, this can be done by the following command.

<!-- language: php -->

    php artisan serve --host=example.dev --port=8080


  [1]: https://laravel.com/docs/5.3/homestead
  [2]: https://laravel.com/docs/5.3/valet

## Server Requirements
The Laravel framework has a few system requirements. Of course, all of these requirements are satisfied by the [Laravel Homestead][1] virtual machine, so it's highly recommended that you use Homestead as your local Laravel development environment.

However, if you are not using Homestead, you will need to make sure your server meets the following requirements:

 - PHP >= 5.6.4
 - OpenSSL PHP Extension
 - PDO PHP Extension
 - Mbstring PHP Extension
 - Tokenizer PHP Extension
 - XML PHP Extension


  [1]: https://laravel.com/docs/5.3/homestead

## Installing Laravel
**Requirements:**  
You need `PHP >= 5.6.4` and `Composer` installed on your machine. You can check version of both by using command:  
**For PHP:**

<!-- language: php -->

    php -v

Output like this:  

<!-- language: php -->

    PHP 7.0.9 (cli) (built: Aug 26 2016 06:17:04) ( NTS )
    Copyright (c) 1997-2016 The PHP Group
    Zend Engine v3.0.0, Copyright (c) 1998-2016 Zend Technologies

**For Composer**  
You can run command on your terminal/CMD:

<!-- language: php -->

    composer --version

Output like this:  

<!-- language: php -->

    composer version 1.2.1 2016-09-12 11:27:19
 

Laravel utilizes [Composer][1] to manage its dependencies. So, before using Laravel, make sure you have Composer installed on your machine.

Via Laravel Installer
---------------------

First, download the Laravel installer using Composer:

<!-- language: php -->

    composer global require "laravel/installer"

Make sure to place the `$HOME/.composer/vendor/bin` directory (or the equivalent directory for your OS) in your $PATH so the `laravel` executable can be located by your system.

Once installed, the `laravel new` command will create a fresh Laravel installation in the directory you specify. For instance, `laravel new blog` will create a directory named `blog` containing a fresh Laravel installation with all of Laravel's dependencies already installed:

<!-- language: php -->

    laravel new blog

Via Composer Create-Project
---------------------------

Alternatively, you may also install Laravel by issuing the Composer `create-project` command in your terminal:

<!-- language: php -->

    composer create-project --prefer-dist laravel/laravel blog

Setup
-----
After you are complete with the Laravel installation, you will need to set `permissions` for the storage and Bootstrap folders.

> **Note:** Setting `permissions` is one of the most important processes to
> complete while installing Laravel.

**Local Development Server**

If you have PHP installed locally and you would like to use PHP's built-in development server to serve your application, you may use the `serve` Artisan command. This command will start a development server at `http://localhost:8000`:

<!-- language: php -->

    php artisan serve

Open your browser request url `http://localhost:8000`

  [1]: https://getcomposer.org/

## Hello World Example (Basic)
Open routes file. Paste the following code in:

<!-- language: php -->

    Route::get('helloworld', function () {
        return '<h1>Hello World</h1>';
    });

after going to route `http://localhost/helloworld` it displays Hello World.

The routes file is located `/routes/web.php`

## Web Server Configuration for Pretty URLs
If you installed `Laravel` via `Composer or the Laravel installer`, below configuration you will need.

**Configuration for Apache**
Laravel includes a `public/.htaccess` file that is used to provide URLs without the `index.php` front controller in the path. Before serving Laravel with Apache, be sure to enable the `mod_rewrite` module so the `.htaccess` file will be honored by the server.

If the `.htaccess` file that ships with Laravel does not work with your Apache installation, try this alternative:

<!-- language: php -->

    Options +FollowSymLinks
    RewriteEngine On
    
    RewriteCond %{REQUEST_FILENAME} !-d
    RewriteCond %{REQUEST_FILENAME} !-f
    RewriteRule ^ index.php [L]
-----------------
**Configuration for Nginx**
If you are using Nginx, the following directive in your site configuration will direct all requests to the  `index.php` front controller:

<!-- language: php -->

    location / {
        try_files $uri $uri/ /index.php?$query_string;
    }

Of course, when using [Homestead][1] or [Valet][2], pretty URLs will be automatically configured.


  [1]: https://laravel.com/docs/5.4/homestead
  [2]: https://laravel.com/docs/5.4/valet

