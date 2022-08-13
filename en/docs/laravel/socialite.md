---
title: "Socialite"
slug: "socialite"
draft: false
images: []
weight: 9958
type: docs
toc: true
---

## Installation
    composer require laravel/socialite

This installation assumes you're using [Composer][1] for managing your dependencies with Laravel, which is a great way to deal with it.


  [1]: http://it-should-link-to-composer-tag-in-the-docs

## Configuration
In your `config\services.php` you can add the following code

    'facebook' => [
        'client_id' => 'your-facebook-app-id',
        'client_secret' => 'your-facebook-app-secret',
        'redirect' => 'http://your-callback-url',
    ],

You'll also need to add the Provider to your `config\app.php`

Look for `'providers' => []` array and, at the end of it, add the following

    'providers' => [
          ...
      
          Laravel\Socialite\SocialiteServiceProvider::class,
    ]

A Facade is also provided with the package. If you would like to make usage of it make sure that the `aliases` array (also in your `config\app.php`) has the following code

    'aliases' => [
        ....
        'Socialite' => Laravel\Socialite\Facades\Socialite::class,
    ]

## Basic Usage - Facade
    return Socialite::driver('facebook')->redirect();

This will redirect an incoming request to the appropriate URL to be authenticated. A basic example would be in a controller

    <?php
    
    namespace App\Http\Controllers\Auth;
    
    use Socialite;

    class AuthenticationController extends Controller {
    
        /**
         * Redirects the User to the Facebook page to get authorization.
         *
         * @return Response
         */
        public function facebook() {
            return Socialite::driver('facebook')->redirect();
        }
    
    }

make sure your `app\Http\routes.php` file has a route to allow an incoming request here.

    Route::get('facebook', 'App\Http\Controllers\Auth\AuthenticationController@facebook');



## Basic Usage - Dependency Injection
    /**
     * LoginController constructor.
     * @param Socialite $socialite
     */
    public function __construct(Socialite $socialite) {
        $this->socialite = $socialite;
    }

Within the constructor of your Controller, you're now able to inject the `Socialite` class that will help you handle login with social networks. This will replace the usage of the Facade.

    /**
     * Redirects the User to the Facebook page to get authorization.
     *
     * @return Response
     */
    public function facebook() {
        return $this->socialite->driver('facebook')->redirect();
    }

## Socialite for API - Stateless
    public function facebook() {
        return $this->socialite->driver('facebook')->stateless()->redirect()->getTargetUrl();
    }

This will return the URL that the consumer of the API must provide to the end user to get authorization from Facebook.

