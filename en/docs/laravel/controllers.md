---
title: "Controllers"
slug: "controllers"
draft: false
images: []
weight: 9991
type: docs
toc: true
---

Instead of defining all of your request handling logic as Closures in route files, you may wish to organise this behaviour using Controller classes. Controllers can group related request handling logic into a single class. Controllers are stored in the `app/Http/Controllers` directory by default.

## Basic Controllers
    <?php
    
    namespace App\Http\Controllers;
    
    use App\User;
    use App\Http\Controllers\Controller;
    
    class UserController extends Controller
    {
        /**
         * Show the profile for the given user.
         *
         * @param  int  $id
         * @return Response
         */
        public function show($id)
        {
            return view('user.profile', ['user' => User::findOrFail($id)]);
        }
    }

You can define a route to this controller action like so:

`Route::get('user/{id}', 'UserController@show');`

Now, when a request matches the specified route URI, the `show` method on the `UserController` class will be executed. Of course, the route parameters will also be passed to the method.

## Controller Middleware
Middleware may be assigned to the controller's routes in your route files:

`Route::get('profile', 'UserController@show')->middleware('auth');`

However, it is more convenient to specify middleware within your controller's constructor. Using the middleware method from your controller's constructor, you may easily assign middleware to the controller's action.

    class UserController extends Controller
    {
        /**
         * Instantiate a new controller instance.
         *
         * @return void
         */
        public function __construct()
        {
            $this->middleware('auth');
    
            $this->middleware('log')->only('index');
    
            $this->middleware('subscribed')->except('store');
        }
    }

## Resource Controller
Laravel resource routing assigns the typical "CRUD" routes to a controller with a single line of code. For example, you may wish to create a controller that handles all HTTP requests for "photos" stored by your application. Using the `make:controller` Artisan command, we can quickly create such a controller:

`php artisan make:controller PhotoController --resource`

This command will generate a controller at `app/Http/Controllers/PhotoController.php`. The controller will contain a method for each of the available resource operations.

## Example of how a Resource Controller look

    <?php
    
    namespace App\Http\Controllers;
    
    use Illuminate\Http\Request;
    
    class PhotoController extends Controller
    {
        /**
         * Display a listing of the resource.
         *
         * @return \Illuminate\Http\Response
         */
        public function index()
        {
            //
        }
    
        /**
         * Show the form for creating a new resource.
         *
         * @return \Illuminate\Http\Response
         */
        public function create()
        {
            //
        }
    
        /**
         * Store a newly created resource in storage.
         *
         * @param  \Illuminate\Http\Request  $request
         * @return \Illuminate\Http\Response
         */
        public function store(Request $request)
        {
            //
        }
    
        /**
         * Display the specified resource.
         *
         * @param  int  $id
         * @return \Illuminate\Http\Response
         */
        public function show($id)
        {
            //
        }
    
        /**
         * Show the form for editing the specified resource.
         *
         * @param  int  $id
         * @return \Illuminate\Http\Response
         */
        public function edit($id)
        {
            //
        }
    
        /**
         * Update the specified resource in storage.
         *
         * @param  \Illuminate\Http\Request  $request
         * @param  int  $id
         * @return \Illuminate\Http\Response
         */
        public function update(Request $request, $id)
        {
            //
        }
    
        /**
         * Remove the specified resource from storage.
         *
         * @param  int  $id
         * @return \Illuminate\Http\Response
         */
        public function destroy($id)
        {
            //
        }
    }

The example of the resource controller shares the method name of those in the table below.

Next, you may register a resourceful route to the controller:

`Route::resource('photos', 'PhotoController');`

This single route declaration creates multiple routes to handle a variety of actions on the resource. The generated controller will already have methods stubbed for each of these actions, including notes informing you of the HTTP verbs and URIs they handle.

## Actions Handled By Resource Controller

| Verb | URI | Action | Route Name |
| --- | --- | --- | --- |
| GET | `/photos` | index | photos.index |
| GET | `/photos/create` | create | photos.create |
| POST | `/photos` | store | photos.store |
| GET | `/photos/{photo}` | show | photos.show |
| GET | `/photos/{photo}/edit` | edit | photos.edit |
| PUT/PATCH | `/photos/{photo}` | update | photos.update |
| DELETE | `/photos/{photo}` | destroy | photos.destroy |

