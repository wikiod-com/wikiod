---
title: "Custom Helper function"
slug: "custom-helper-function"
draft: false
images: []
weight: 9951
type: docs
toc: true
---

Adding custom helpers can assist you with your development speed. There are a few things to take into consideration while writing such helper functions though, hence this tutorial.

Just a few pointers:

- We've put the function definitions within a check (`function_exists`) to prevent exceptions when the service provider is called twice.
- An alternative way is registering the helpers file from the `composer.json` file. You can copy the logic from [the laravel framework itself](https://github.com/laravel/framework/blob/5.3/src/Illuminate/Support/composer.json#L31).

## HelpersServiceProvider.php
Now let's create a service provider. Let's put it under `app/Providers`:

    <?php
    
    namespace App\Providers;

    class HelpersServiceProvider extends ServiceProvider
    {
        public function register()
        {
            require_once __DIR__ . '/../Helpers/document.php';
        }
    }

The above service provider load the helpers file and registers your custom function automatically. Please make sure you register this HelpersServiceProvider in your `config/app.php` under `providers`:

    'providers' => [
         // [..] other providers
         App\Providers\HelpersServiceProvider::class,
    ]

## document.php
    <?php

    if (!function_exists('document')) {
        function document($text = '') {
            return $text;
        }
    }

Create a helpers.php file, let's assume for now it lives in `app/Helpers/document.php`. You can put many helpers in one file (this is how Laravel does it) or you can split them up by name.

## Use
Now you can use the function `document()` everywhere in your code, for example in blade templates. This example only returns the same string it receives as an argument

    <?php
    Route::get('document/{text}', function($text) { 
        return document($text);
    });

Now go to `/document/foo` in your browser (use `php artisan serve` or `valet`), which will return `foo`.

