---
title: "Authentication"
slug: "authentication"
draft: false
images: []
weight: 9951
type: docs
toc: true
---

## Multi Authentication
Laravel allows you to use multiple Authentication types with specific guards.

In laravel 5.3 multiple authentication is little different from Laravel 5.2

I will explain how to implement multiauthentication feature in 5.3

First you need two different user Model

    cp App/User.php App/Admin.php

change class name to Admin and set namespace if you use models different. it should look like
    
**App\Admin.php**

    <?php
    
    namespace App;
    
    use Illuminate\Foundation\Auth\User as Authenticatable;
    use Illuminate\Notifications\Notifiable;
    
    class Admin extends Authenticatable
    {
        use Notifiable;
    
        protected $fillable = ['name', 'email', 'password'];
        protected $hidden   = ['password', 'remember_token'];
    
    }

Also you need create a migration for admin

    php artisan make:migration create_admins_table

then edit migration file with contents of default user migration. Looks like this

    <?php
    
    use Illuminate\Database\Migrations\Migration;
    use Illuminate\Database\Schema\Blueprint;
    use Illuminate\Support\Facades\Schema;
    
    class CreateAdminsTable extends Migration
    {
        /**
         * Run the migrations.
         *
         * @return void
         */
        public function up()
        {
            Schema::create('admins', function (Blueprint $table) {
                $table->increments('id');
                $table->string('name');
                $table->string('email')->unique();
                $table->string('password');        
                $table->rememberToken();
                $table->timestamps();
    
                $table->softDeletes();
            });
        }
    
        /**
         * Reverse the migrations.
         *
         * @return void
         */
        public function down()
        {
            Schema::drop('admins');
        }
    }

edit **config/auth.php**

    'guards'    => [
            'web'   => [
                'driver'   => 'session',
                'provider' => 'users',
            ],
    
            'api'   => [
                'driver'   => 'token',
                'provider' => 'users',
            ],
            //Add Admin Guard
            'admin' => [
                'driver'   => 'session',
                'provider' => 'admins',
            ],
        ],
and 

    'providers' => [
            'users'  => [
                'driver' => 'eloquent',
                'model'  => App\User::class,
            ],
            //Add Admins Provider
            'admins' => [
                'driver' => 'eloquent',
                'model'  => App\Admin::class,
            ],
        ],

Notice that we add two entry. one in **guards** variable one in **providers** variable.

And this is how you use the other guard then "web"

My App\Http\Controllers\Admin\LoginController

    <?php
    
    namespace App\Http\Controllers\Admin;
    
    use App\Http\Controllers\Controller;
    use Illuminate\Foundation\Auth\AuthenticatesUsers;
    use Illuminate\Support\Facades\Auth;
    
    class AuthController extends Controller
    {
    
        use AuthenticatesUsers;
    
        protected $guard = 'admin';
    
        protected $redirectTo = '/admin/';
    
        public function showLoginForm()
        {
            return view('admin.login');
        }
    
        protected function guard()
        {
            return Auth::guard($this->guard);
        }
    
    }

this needs little explanation. 

in a nutshell **Auth::guard('admin')** will allow you to use auth methods (such as login, logout, register etc.) with your admin guard.

For example 

    Auth::guard('admin')->login($user)

will search $user in admins table and login with the user while

    Auth::login($user)

will works normally with users table. Default guard is specified in **config/auth.php**
with *defaults* array. 
In fresh laravel it is "web" .

In controller you have to implement methods from AuthenticatesUsers to show your custom view paths. And you need implement other functions such as guard to use your new user guards. 

In this example my admin login is **admin/login.blade**

And by implementing guard() function to return **Auth::guard('admin')** all AuthenticatesUsers trait methods works with "admin" guard.
    

In earlier versions of laravel, this is little different from 5.3 

in 5.2 getGuard function returns $guard variable from class and main function (login) use it in 

    Auth::guard($guard)->attempt(...)

in 5.3 guard function returns whole Auth::guard() and main function use it like 

    $this->guard()->attempt(...)





