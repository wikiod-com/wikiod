---
title: "Directory Structure"
slug: "directory-structure"
draft: false
images: []
weight: 9962
type: docs
toc: true
---

## Change default app directory
There are use cases when you might want to rename your app directory to something else. In Laravel4 you could just change a config entry, here's one way to do it in Laravel5.

In this example we'll be renaming the `app` directory to `src`.

## Override Application class
The directories name `app` is hardcoded into the core Application class, so it has to be overridden. Create a new file `Application.php`. I prefer to keep mine in the `src` directory (the one we'll be replacing app with), but you can place it elsewhere.

Here's how the overridden class should look like. If you want a different name, just change the string `src` to something else.

    namespace App;

    class Application extends \Illuminate\Foundation\Application
    {
        /**
         * @inheritdoc
         */
        public function path($path = '')
        {
            return $this->basePath . DIRECTORY_SEPARATOR . 'src' . ($path ? DIRECTORY_SEPARATOR . $path : $path);
        }
    }
Save the file. We're done with it.

## Calling the new class
Open up `bootstrap/app.php` and locate

    $app = new Illuminate\Foundation\Application(
        realpath(__DIR__.'/../')
    );

We'll be replacing it with this

    $app = new App\Application(
        realpath(__DIR__.'/../')
    );

## Composer
Open up your `composer.json` file and change autoloading to match your new location

    "psr-4": {
        "App\\": "src/"
    }

And finally, in the command line run `composer dump-autoload` and your app should be served from the `src` directory.

## Change the  Controllers directory
if we want to change the `Controllers` directory we need:

 1. Move and/or rename the default `Controllers` directory where we want it. For example from `app/Http/Controllers` to `app/Controllers`


 2. Update all the namespaces of the files inside the `Controllers` folder, making they adhere to the new path, respecting the PSR-4 specific. 
 

 3. Change the namespace that is applied to the `routes.php` file, by editing `app\Providers\RouteServiceProvider.php` and change this:

<!-- language: php -->

    protected $namespace = 'App\Http\Controllers';

to this:


<!-- language: php -->

    protected $namespace = 'App\Controllers';



