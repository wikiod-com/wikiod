---
title: "Form Request(s)"
slug: "form-requests"
draft: false
images: []
weight: 9977
type: docs
toc: true
---

Custom requests (or Form Requests) are useful in situations when one wants to **authorize** & **validate** a request before hitting the controller method.

One may think of two practical uses, **creating** & **updating** a record while each action has a different set of validation (or authorization) rules.

Using Form Requests is trivial, one has to type-hint the request class in method.

## Syntax
    

 - php artisan make:request name_of_request

Requests are useful when separating your validation from Controller. It also allows you to check if the request is authorized. 

## Creating Requests
    php artisan make:request StoreUserRequest

    php artisan make:request UpdateUserRequest

>**Note**: You can also consider using names like **StoreUser** or **UpdateUser** (without **Request** appendix) since your FormRequests are placed in folder `app/Http/Requests/`.


## Using Form Request
Lets say continue with User example (you may have controller with store method and update method). To use FormRequests you use type-hinting the specific request.

    ...
    
    public function store(App\Http\Requests\StoreRequest $request, App\User $user) { 
        //by type-hinting the request class, Laravel "runs" StoreRequest 
        //before actual method store is hit
    
        //logic that handles storing new user 
        //(both email and password has to be in $fillable property of User model
        $user->create($request->only(['email', 'password']));
        return redirect()->back();
    }

    ...
    
    public function update(App\Http\Requests\UpdateRequest $request, App\User $users, $id) { 
        //by type-hinting the request class, Laravel "runs" UpdateRequest 
        //before actual method update is hit
    
        //logic that handles updating a user 
        //(both email and password has to be in $fillable property of User model
        $user = $users->findOrFail($id);
        $user->update($request->only(['password']));
        return redirect()->back();
    }

## Handling Redirects after Validation
Sometimes you may want to have some login to determine where the user gets redirected to after submitting a form. Form Requests give a variety of ways.

By default there are 3 variables declared in the Request `$redirect`, `$redirectRoute` and  `$redirectAction`.

On top of those 3 variables you can override the main redirect handler `getRedirectUrl()`.

A sample request is given below explaining what you can do.


    <?php namespace App;

    use Illuminate\Foundation\Http\FormRequest as Request;

    class SampleRequest extends Request {

        // Redirect to the given url
        public $redirect;

        // Redirect to a given route
        public $redirectRoute;

        // Redirect to a given action
        public $redirectAction;


        /**
         * Get the URL to redirect to on a validation error.
         *
         * @return string
         */
        protected function getRedirectUrl()
        {

            // If no path is given for `url()` it will return a new instance of `Illuminate\Routing\UrlGenerator`

            // If your form is down the page for example you can redirect to a hash
            return url()->previous() . '#contact';

            //`url()` provides several methods you can chain such as

            // Get the current URL
            return url()->current();

            // Get the full URL of the current request
            return url()->full();

            // Go back
            return url()->previous();

            // Or just redirect back
            return redirect()->back();
        }


        /**
         * Get the validation rules that apply to the request.
         *
         * @return array
         */
        public function rules()
        {
            return [];
        }

        /**
         * Determine if the user is authorized to make this request.
         *
         * @return bool
         */
        public function authorize()
        {
            return true;
        }
    }


