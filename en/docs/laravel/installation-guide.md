---
title: "Installation Guide"
slug: "installation-guide"
draft: false
images: []
weight: 9980
type: docs
toc: true
---

This section provides an overview of what laravel-5.4 is, and why a developer might want to use it.

It should also mention any large subjects within laravel-5.4, and link out to the related topics.  Since the Documentation for laravel-5.4 is new, you may need to create initial versions of those related topics.

## Installation
Detailed instructions on getting laravel set up or installed.

[composer][3] is required for installing laravel easily.

There are 3 methods of installing laravel in your system:

 1. **Via Laravel Installer**

    Download the Laravel installer using `composer`

    <!-- language: php -->

        composer global require "laravel/installer"

    Before using composer we need to add `~/.composer/vendor/bin` to `PATH`. After installation has finished we can use `laravel new` command to create a new project in `Laravel`.

    Example:

    <!-- language: php -->

        laravel new {folder name}

    This command creates a new directory named as `site` and a fresh `Laravel` installation with all other dependencies are installed in the directory.

 2. **Via Composer Create-Project**

    You can use the command in the `terminal` to create a new `Laravel app`:

    <!-- language: php -->

        composer create-project laravel/laravel {folder name}

 3. **Via Download**

    Download [Laravel][1] and unzip it.

    1. `composer install`
    2. Copy `.env.example` to `.env` via `teminal` or manually.
        ```bash
        cp .env.example .env
        ```
    3. Open `.env` file and set your database, email, pusher, etc. (if needed)
    4. `php artisan migrate` (if database is setup)
    5. `php artisan key:generate`
    6. `php artisan serve`
    7. Go to [localhost:8000][4] to view the site

[Laravel docs][2]


  [1]: https://github.com/laravel/laravel/
  [2]: https://laravel.com/docs/5.4#installing-laravel
  [3]: https://getcomposer.org/download/
  [4]: http://localhost:8000/

## Hello World Example (Basic)
Accessing pages and outputting data is fairly easy in Laravel. All of the page routes are located in `app/routes.php`. There are usually a few examples to get you started, but we're going to create a new route. Open your `app/routes.php`, and paste in the following code:

<!-- language: php -->
  
    Route::get('helloworld', function () {
        return '<h1>Hello World</h1>';
    });

This tells Laravel that when someone accesses `http://localhost/helloworld` in a browser, it should run the function and return the string provided. 


## Hello World Example With Views and Controller
Assuming we have a working laravel application running in, say, "mylaravel.com",we want our application to show a "Hello World" message when we hit the URL `http://mylaravel.com/helloworld` . It involves the creation of two files (the view and the controller) and the modification of an existing file, the router.

<h3>The view</h3>
First off , we open a new blade view file named `helloview.blade.php` with the "Hello World" string. Create it in the directory app/resources/views

<!-- language: php -->

    <h1>Hello, World</h1>

<h3>The controller</h3>
Now we create a controller that will manage the display of that view with the "Hello World" string. We'll use artisan in the command line.

<!-- language: php -->

    $> cd your_laravel_project_root_directory
    $> php artisan make:controller HelloController 

That will just create a file (`app/Http/Controllers/HelloController.php`) containing the class that is our new controller `HelloController`. 

   Edit that new file and write a new method `hello` that will display the view we created before.

<!-- language: php -->

    public function hello()
    {
        return view('helloview'); 
    }
That 'helloview' argument in the view function is just the name of the view file without the trailing ".blade.php". Laravel will know how to find it.

Now when we call the method `hello` of the controller `HelloController` it will display the message. But how do we link that to a call to `http://mylaravel.com/helloworld` ? With the final step, the routing.
<h3>The router</h3>

Open the existing file `app/routes/web.php` (in older laravel versions `app/Http/routes.php`) and add this line:

<!-- language: php -->

    Route::get('/helloworld', 'HelloController@hello');

which is a very self-explaining command saying to our laravel app: "When someone uses the `GET` verb to access '/helloworld' in this laravel app, return the results of calling the function `hello` in the `HelloController` controller.



