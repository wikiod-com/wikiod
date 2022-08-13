---
title: "Installation"
slug: "installation"
draft: false
images: []
weight: 9817
type: docs
toc: true
---

## Installation
Laravel applications are installed and managed with [Composer][1], a popular PHP dependency manager. There are two ways to create a new Laravel application.

## Via Composer

    $ composer create-project laravel/laravel [foldername]

Or 

    $ composer create-project --prefer-dist laravel/laravel [foldername]

Replace **[foldername]** with the name of the directory you want your new Laravel application installed to. It must not exist before installation. You may also need to add the Composer executable to your system path.

If want to create a Laravel project using a specific version of the framework, you can provide a version pattern, otherwise your project will use the latest available version.

If you wanted to create a project in Laravel 5.2 for example, you'd run:

    $ composer create-project --prefer-dist laravel/laravel 5.2.*

**Why --prefer-dist**

There are two ways of downloading a package: `source` and `dist`. For stable versions Composer will use the `dist` by default. The `source` is a version control repository. If `--prefer-source` is enabled, Composer will install from source if there is one.

`--prefer-dist` is the opposite of `--prefer-source`, and tells Composer to install from `dist` if possible. This can speed up installs substantially on build servers and in other use cases where you typically do not run vendor updates. It also allows avoiding problems with Git if you do not have a proper setup.
    
## Via the Laravel installer

Laravel provides a helpful command line utility to quickly create Laravel applications. First, install the installer:

    $ composer global require laravel/installer

> You have to make sure that the Composer binaries folder is within your $PATH variable to execute the Laravel installer.
>
> First, look if it already is in your $PATH variable
>
> ```echo $PATH```
>
> If everything is correct, the output should contain something like this:
>
> ```Users/yourusername/.composer/vendor/bin```
>
> If not, edit your ```.bashrc``` or, if your using ZSH, your ```.zshrc``` so it contains the path to your Composer vendor directory.

Once installed, this command will create a fresh Laravel installation in the directory you specify. 

    laravel new [foldername]

You can also use `.` (a dot) in place of **[foldername]** to create the project in the current working directory without making a sub-directory.

## Running the application

Laravel comes bundled with a PHP-based web server which can be started by running

    $ php artisan serve

By default, the HTTP server will use port 8000, but if the port is already in use or if you want to run multiple Laravel applications at once, you can use the `--port` flag to specify a different port:

    $ php artisan serve --port=8080

The HTTP server will use `localhost` as the default domain for running the application, but you can use the `--host` flag to specify a different address:

    $ php artisan serve --host=192.168.0.100 --port=8080

### Using a different server 

If you prefer to use a different web server software, some configuration files are provided for you inside the `public` directory of your project; `.htaccess` for Apache and `web.config` for ASP.NET. For other software such as NGINX, you can convert the Apache configurations using various online tools.

---

The framework needs the web server user to have write permissions on the following directories:
- `/storage`
- `/bootstrap/cache`

On *nix operating systems this can be achieved by

    chown -R www-data:www-data storage bootstrap/cache
    chmod -R ug+rwx storage bootstrap/cache
   
(where `www-data` is the name and group of the web server user)

--- 
The web server of your choice should be configured to serve content from your project's `/public` directory, which is usually done by setting it as the document root. The rest of your project should not be accessible through your web server.

If you set everything up properly, navigating to your website's URL should display the default landing page of Laravel.

[1]: https://getcomposer.org/

## Requirements
The Laravel framework has the following requirements:

<!-- if version [gte 5.3] -->
* PHP >= 5.6.4
* XML PHP Extension
* PDO PHP Extension
* OpenSSL PHP Extension
* Mbstring PHP Extension
* Tokenizer PHP Extension
<!-- end version if -->

<!-- if version [eq 5.1 (LTS)] [eq 5.2] -->
* PHP >= 5.5.9
* PDO PHP Extension
* Laravel 5.1 is the first version of Laravel to support PHP 7.0.
<!-- end version if -->

<!-- if version [eq 5.0] -->
* PHP >= 5.4, PHP < 7
* OpenSSL PHP Extension
* Tokenizer PHP Extension
* Mbstring PHP Extension
* JSON PHP extension (only on PHP 5.5)
<!-- end version if -->

<!-- if version [eq 4.2] -->
* PHP >= 5.4
* Mbstring PHP Extension
* JSON PHP extension (only on PHP 5.5)
<!-- end version if -->


## Hello World Example (Basic)
Open routes file. Paste the following code in:

    Route::get('helloworld', function () {
        return '<h1>Hello World</h1>';
    });

after going to route `localhost/helloworld` it displays **Hello World**.

The routes file is located:
 
<!-- if version [gte 5.3] -->
For Web

    routes/web.php

For APIs

    routes/api.php

<!-- end version if -->

<!-- if version [eq 5.2] [eq 5.1 (LTS)] [eq 5.0] -->
    app/Http/routes.php
<!-- end version if -->

<!-- if version [eq 4.2] -->
    app/routes.php 
<!-- end version if -->

## Hello World Example (Using Controller and View)
 1. Create a Laravel application:

        $ composer create-project laravel/laravel hello-world

 2. Navigate to the project folder, e.g.

        $ cd C:\xampp\htdocs\hello-world

 3. Create a controller:

        $ php artisan make:controller HelloController --resource

   > This will create the file __app/Http/Controllers/HelloController.php__. The `--resource` option will generate CRUD methods for the controller, e.g. index, create, show, update.

 4. Register a route to HelloController's `index` method. Add this line to __app/Http/routes.php__ *(version 5.0 to 5.2)* or __routes/web.php__ *(version 5.3)*:

<!-- language: php -->

        Route::get('hello', 'HelloController@index');

> To see your newly added routes, you can run `$ php artisan route:list`

 5. Create a Blade template in the `views` directory:

    __resources/views/hello.blade.php:__

        <h1>Hello world!</h1>

 6. Now we tell index method to display the __hello.blade.php__ template:

    __app/Http/Controllers/HelloController.php__

<!-- language: php -->

        <?php
        
        namespace App\Http\Controllers;
        
        use Illuminate\Http\Request;
        
        use App\Http\Requests;
        
        class HelloController extends Controller
        {
            /**
             * Display a listing of the resource.
             *
             * @return \Illuminate\Http\Response
             */
            public function index()
            {
                return view('hello');
            }
        
            // ... other resources are listed below the index one above

You can serve your app using the following PHP Artisan Command: **`php artisan serve`**; it will show you the address at which you can access your application *(usually at http://localhost:8000 by default)*.

Alternatively, you may head over directly to the appropriate location in your browser; *in case you are using a server like XAMPP (either: http://localhost/hello-world/public/hello should you have installed your Laravel instance, **`hello-world`**, directly in your __xampp/htdocs__ directory as in: having executed the step 1 of this Hello Word from your command line interface, pointing at your __xampp/htdocs__ directory)*.

## Installation using LaraDock (Laravel Homestead for Docker)
LaraDock is a Laravel Homestead like development environment but for Docker instead of Vagrant. https://github.com/LaraDock/laradock

# Installation

*Requires Git and Docker

 Clone the LaraDock repository:
        
A.  If you already have a Laravel project, clone this repository on your Laravel root directory: 

    git submodule add https://github.com/LaraDock/laradock.git

B.  If you don't have a Laravel project, and you want to install Laravel from Docker, clone this repo anywhere on your machine: 

    git clone https://github.com/LaraDock/laradock.git

# Basic Usage

1. Run Containers: (Make sure you are in the laradock folder before running the docker-compose commands).

   Example: Running NGINX and MySQL: `docker-compose up -d  nginx mysql`

   There are a list of available containers you can select to create your own combinations.

   `nginx`, `hhvm`, `php-fpm`, `mysql`, `redis`, `postgres`, `mariadb`, `neo4j`, `mongo`, `apache2`, `caddy`, `memcached`, `beanstalkd`, `beanstalkd-console`, `workspace`

2. Enter the Workspace container, to execute commands like (Artisan, Composer, PHPUnit, Gulp, ...).

    `docker-compose exec workspace bash`

3. If you don't have a Laravel project installed yet, follow the step to install Laravel from a Docker container.

   a. Enter the Workspace container.

   b. Install Laravel. `composer create-project laravel/laravel my-cool-app "5.3.*"`

4.  Edit the Laravel configurations. Open your Laravel's .env file and set the DB_HOST to your mysql:

    `DB_HOST=mysql`
5. Open your browser and visit your localhost address.

