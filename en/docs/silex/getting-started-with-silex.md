---
title: "Getting started with silex"
slug: "getting-started-with-silex"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation or Setup
Consider following directory structure to comply with best practices:

    [ProjectRoot]
        |---[app]
        |    |---[resources]
        |    routes.php
        |---[web]
        |    |---[resources]
        |    |    |---[css]
        |    |    |---[img]
        |    |    |---[js]
        |    .htaccess
        |    index.php
        |---[src]
        |---[var]
        |---[vendor]
        composer.json
        composer.lock

[Composer][1] is the most flexible way to get started with Silex. Create a directory to host your Silex application (Named `ProjectRoot` above), `cd` to that directory and run the following command to create a `composer.json` file:

    composer require silex/silex "~2.0"
This will add some files and directories into `vendor` directory, under `ProjectRoot`.

After that, all you need to do is require the `vendor/autoload.php` file and create an instance of `Silex\Application` in your `index.php` file under `ProjectRoot/web`. After your controller definitions (`routes.php`), call the `run` method on your application:

**ProjectRoot/web/index.php**

    require_once __DIR__ . '/../vendor/autoload.php';
    
    $app = new Silex\Application();
    require_once __DIR__ . '/../app/routes.php';

    $app->run();
**ProjectRoot/app/routes.php**

    $app->get("/", function () {
        return new \Symfony\Component\HttpFoundation\Response('Hello World!');
    });
**ProjectRoot/web/.htaccess**

    <IfModule mod_rewrite.c>
        Options -MultiViews
        RewriteEngine On
        RewriteBase /
        RewriteCond %{REQUEST_FILENAME} !-d
        RewriteCond %{REQUEST_FILENAME} !-f
        RewriteRule ^ index.php [QSA,L]
    </IfModule>

  [1]: https://getcomposer.org/

## Hello world
Create `web` directory in same folder as the `vendor` directory.
Create `index.php` file in `web` directory with contents

```php
<?php
// web/index.php

require_once __DIR__.'/../vendor/autoload.php';

$app = new Silex\Application();

$app->get("/", function () {
    return "Hello world!";
});

$app->get("/hello/{name}", function ($name) use ($app) {
    return "Hello ".$app->escape($name);
});

$app->run();
```

To start app using PHP [built-in server][1] run

    php -S localhost:8080 -t web

Now you can open the browser and navigate to `http://localhost:8080`, to see 

>Hello World!

We also defined one dynamic route. 
Navigate to `http://localhost:8080/hello/<YOUR_NAME>` replacing `<YOUR_NAME>` with your own name to be greeted by your first Silex app.


  [1]: https://www.wikiod.com/php/getting-started-with-php#PHP built-in server

