---
title: "Validation"
slug: "validation"
draft: false
images: []
weight: 9675
type: docs
toc: true
---

## Parameters
| Parameter | Details |
|-----------|---------|
| required  | The field is required |
| sometimes | Run validation checks against a field only if that field is present in the input array
| email     | The input is a valid email |
| max:value   | The input value should be below the maximum value |
| unique:db_table_name| The input value should be unique in the provided database table name |
| accepted | Yes / On / 1 true, useful for checking TOS |
| active_url | Must be a valid URL according to [checkdnsrr][1] |
| after _:date_ | Field under validation must provide a value after the given date |
| alpha| The field under validation must be entirely alphabetic characters. |
| alpha_dash| The field under validation may have alpha-numeric characters, as well as dashes and underscores. |
| alpha_num| The field under validation must be entirely alpha-numeric characters. |
| array | Must be a PHP [array][2] |
| before _:date_ | The field must be a value under the given date |
| between:min,max| The input value should be in between minimum (min) and maximum (max) value |
| boolean | The field under validation must be able to be cast as a boolean. Accepted input are `true`, `false`, `1`, `0`, `"1"`, and `"0"`. |
| confirmed | The field under validation must have a matching field of `foo_confirmation`. For example, if the field under validation is `password`, a matching `password_confirmation` field must be present in the input. |
| date      | The field under validation must be a valid date according to the [strtotime][3] PHP function. |
| integer   | The field under validation must be an [integer][4]
| string    | The field under validation must be a [string][5] type.


  [1]: http://php.net/checkdnsrr
  [2]: http://php.net/array
  [3]: http://php.net/strtotime
  [4]: http://php.net/manual/en/language.types.integer.php
  [5]: http://php.net/manual/en/ref.strings.php

## Basic Example
<!-- Language-All: php -->
You can validate request data using the `validate` method (available in the base Controller, provided by the `ValidatesRequests` trait). 

If the rules pass, your code will keep executing normally; however, if validation fails, an error response containing the validation errors will automatically be sent back:
* for typical HTML form requests, the user will be redirected to the previous page, with the form keeping the submitted values
* for requests that expect a JSON response, a HTTP response with code 422 will be generated

For example, in your `UserController`, you might be saving a new user in the `store` method, which would need validation before saving. 

    /**
     * @param  Request  $request
     * @return Response
     */
    public function store(Request $request) {
        $this->validate($request, [
            'name' => 'required',
            'email' => 'email|unique:users|max:255'
        ],
        // second array of validation messages can be passed here
        [
            'name.required' => 'Please provide a valid name!',
            'email.required' => 'Please provide a valid email!',
        ]);
    
        // The validation passed
    }

In the example above, we validate that the `name` field exists with non-empty value. Secondly, we check that the `email` field has a valid e-mail format, is unique in the database table "users", and has maximum length of 255 characters.

> The `|` (pipe) character combines different validation rules for one field.

Sometimes you may wish to stop running validation rules on an attribute after the first validation failure. To do so, assign the `bail` rule to the attribute:

    $this->validate($request, [
        'name' => 'bail|required',
        'email' => 'email|unique:users|max:255'
    ]);

The complete list of available validation rules can be found in the [parameters section below][1].



  [1]: https://www.wikiod.com/laravel/validation

## Other Validation Approaches
<!-- Language-all: php -->
**1) Form Request Validation**

You may create a "form request" which can hold the authorization logic, validation rules, and error messages for a particular request in your application. 

The `make:request` Artisan CLI command generates the class and places it in the `app/Http/Requests` directory:

    php artisan make:request StoreBlogPostRequest

The `authorize` method can be overridden with the authorization logic for this request:

    public function authorize()
    {        
        return $this->user()->can('post');
    }

The `rules` method can be overridden with the specific rules for this request:

    public function rules()
    {
        return [
            'title' => 'required|unique:posts|max:255',
            'body' => 'required',
        ];
    }


The `messages` method can be overridden with the specific messages for this request:

    public function messages()
    {
        return [
            'title.required' => 'A title is required',
            'title.unique' => 'There is another post with the same title',
            'title.max' => 'The title may not exceed :max characters',
            'body.required' => 'A message is required',
        ];
    }

In order to validate the request, just type-hint the specific request class on the corresponding controller method. If validation fails, an error response will be sent back.

    public function store(StoreBlogPostRequest $request)
    {
        // validation passed
    }
---

**2) Manually Creating Validators**

For more flexibility, you may want to create a Validator manually, and handle the failed validation directly: 

    <?php    
    namespace App\Http\Controllers;
    
    use Validator;
    use Illuminate\Http\Request;
    use App\Http\Controllers\Controller;
    
    class PostController extends Controller
    {
        public function store(Request $request)
        {
            $validator = Validator::make($request->all(), [
                'title' => 'required|unique:posts|max:255',
                'body' => 'required',
            ]);
    
            if ($validator->fails()) {
                return redirect('post/create')
                        ->withErrors($validator)
                        ->withInput();
            }
    
            // Store the blog post...
        }
    }

**2) Fluently creating rules**

Occasionally you might need to create unique rules on the fly, working with the `boot()` method within a Service Provider might be over the top, as of Laravel 5.4 you can create new rules fluently by using the `Rule` class.

As an example we are going to work with the `UserRequest` for when you want to insert or update a user. For now we want a name to be required and the email address must be unique. The problem with using the `unique` rule is that if you are editing a user, they might keep the same email, so you need to exclude the current user from the rule. The following example shows how you can easily do this by utilising the new `Rule` class.


    <?php
    namespace App\Http\Requests;
    use Illuminate\Foundation\Http\FormRequest;
    use Illuminate\Http\Request;
    use Illuminate\Validation\Rule;
    
    class UserRequest extends FormRequest
    {
        /**
         * Determine if the user is authorized to make this request.
         *
         * @return bool
         */
        public function authorize()
        {
            return true;
        }
    
        /**
         * Get the validation rules that apply to the request.
         *
         * @return array
         */
        public function rules(Request $request)
        {
            $id = $request->route()->getParameter('user');
    
            return [
                'name'           =>  'required',
                
                // Notice the value is an array and not a string like usual
                'email'         =>  [
                    'required',
                    Rule::unique('users')->ignore($id)
                ]
            ];
        }
    }

## Array Validation
<!-- Language-All: php -->
Validating array form input fields is very simple.

Suppose you have to validate each name, email and father name in a given array. You could do the following:

<!-- language: lang-php -->

    $validator = \Validator::make($request->all(), [
        'name.*'       => 'required', 
        'email.*'      => 'email|unique:users',
        'fatherName.*' => 'required'
    ]);
    
    if ($validator->fails()) {
        return back()->withInput()->withErrors($validator->errors());
    }
    
Laravel displays default messages for validation. However, if you want custom messages for array based fields, you can add the following code:

<!-- language: lang-php -->

    [
        'name.*' => [
            'required' => 'Name field is required',
        ],
        'email.*' => [
            'unique'   => 'Unique Email is required',
        ],
        'fatherName.*' => [
            'required' => 'Father Name required',
        ]
    ]

Your final code will look like this:

<!-- language: lang-php -->

    $validator = \Validator::make($request->all(), [
        'name.*'       => 'required', 
        'email.*'      => 'email|unique:users',
        'fatherName.*' => 'required',
    ], [
        'name.*'       => 'Name Required',
        'email.*'      => 'Unique Email is required',
        'fatherName.*' => 'Father Name required',
    ]);
    
    if ($validator->fails()) {
        return back()->withInput()->withErrors($validator->errors());
    }

## Single Form Request Class for POST, PUT, PATCH
<!-- Language-all: php -->

Following the ['Form Request Validation'][1] example, the same Request Class can be used for `POST`, `PUT`, `PATCH` so you do not have to create another class using the same/similar validations. This comes in handy if you have attributes in your table that are unique.

    /**
     * Get the validation rules that apply to the request.
     *
     * @return array
     */
    public function rules() {
        switch($this->method()) {
            case 'GET':
            case 'DELETE':
                return [];
            case 'POST':
                return [
                    'name'     => 'required|max:75|unique',
                    'category' => 'required',
                    'price'    => 'required|between:0,1000',
                ];
            case 'PUT':
            case 'PATCH':
                return [
                    'name'     => 'required|max:75|unique:product,name,' . $this->product,
                    'category' => 'required',
                    'price'    => 'required|between:0,1000',
                ];
            default:break;
        }
    }

Starting from the top, our switch statement is going to look at the method type of the request (`GET`, `DELETE`, `POST`, `PUT`, `PATCH`). 

Depending on the method will return the array of rules defined. If you have a field that is unique, such as the `name` field in the example, you need to specify a particular id for the validation to ignore.

    'field_name' => 'unique:table_name,column_name,' . $idToIgnore`

If you have a primary key labeled something other than `id`, you will specify the primary key column as the fourth parameter.

    'field_name' => 'unique:table_name,column_name,' . $idToIgnore . ',primary_key_column'

In this example, we are using `PUT` and passing to the route (`admin/products/{product}`) the value of the product id. So `$this->product` will be equal to the `id` to ignore.

Now your `PUT|PATCH` and `POST` validation rules do not need to be the same. Define your logic that fits your requirements. This technique allows you to reuse the custom messages you may have defined within the custom Form Request Class.


  [1]: https://www.wikiod.com/laravel/validation#Other Validation Approaches

## Error messages
Customizing error messages
--

The `/resources/lang/[lang]/validation.php` files contain the error messages to be used by the validator. You can edit them as needed.

Most of them have placeholders which will be automatically replaced when generating the error message. 

For example, in `'required' => 'The :attribute field is required.'`, the `:attribute` placeholder will be replaced by the field name (alternatively, you can also customize the display value of each field in the `attributes` array in the same file).

*Example*

message configuration:

    'required' => 'Please inform your :attribute.',
    //...
    'attributes => [
        'email' => 'E-Mail address'
    ]

rules:

    `email' => `required`

resulting error message:
> "Please inform your E-Mail address."

---

Customising error messages within a Request class
--

The Request class has access to a `messages()` method which should return an array, this can be used to override messages without having to go into the lang files. For example if we have a custom `file_exists` validation you can messages like below.

    class SampleRequest extends Request {
    
        /**
         * Get the validation rules that apply to the request.
         *
         * @return array
         */
        public function rules()
        {
            return [
                'image' =>  'required|file_exists'
            ];
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
    
        public function messages()
        {
            return [
                'image.file_exists' =>  'That file no longer exists or is invalid'
            ];
        }
    
    }

---

Displaying error messages
--

The validation errors are flashed to the session, and are also available in the `$errors` variable, which is automatically shared to all views.

Example of displaying the errors in a Blade view:

    @if (count($errors) > 0)
        <div class="alert alert-danger">
            <ul>
                @foreach ($errors->all() as $error)
                    <li>{{ $error }}</li>
                @endforeach
            </ul>
        </div>
    @endif

## Custom Validation Rules
<!-- language-all: php -->

If you want to create a custom validation rule, you can do so for instance in the `boot` method of a service provider, via the Validator facade.

    <?php
    namespace App\Providers;
    
    use Illuminate\Support\ServiceProvider;
    use Validator;
    
    class AppServiceProvider extends ServiceProvider
    {
        public function boot()
        {
            Validator::extend('starts_with', function($attribute, $value, $parameters, $validator) {
                return \Illuminate\Support\Str::startsWith($value, $parameters[0]);
            });
    
            Validator::replacer('starts_with', function($message, $attribute, $rule, $parameters) {
                return str_replace(':needle', $parameters[0], $message);
            });
        }
    }

The `extend` method takes a string which will be the name of the rule and a function which in turn will be passed the name of the attribute, the value being validated, an array of the rule parameters, and the validator instance, and should return whether the validation passes. In this example, we are checking if the value string starts with a given substring.

The error message for this custom rule can be set as usual in the `/resources/lang/[lang]/validation.php` file, and can contain placeholders, for instance, for parameters values:

    'starts_with' => 'The :attribute must start with :needle.'

The `replacer` method takes a string which is the name of the rule and a function which in turn will be passed the original message (before replacing), the name of the attribute, the name of the rule, and an array of the rule parameters, and should return the message after replacing the placeholders as needed.

Use this rule as any other:

    $this->validate($request, [
        'phone_number' => 'required|starts_with:+'
    ]);

