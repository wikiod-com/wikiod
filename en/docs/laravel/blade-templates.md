---
title: "Blade Templates"
slug: "blade-templates"
draft: false
images: []
weight: 9851
type: docs
toc: true
---

Laravel supports Blade templating engine out of the box.
The Blade templating engine allows us to create master templates and child templating loading content from master templates, we can have variables, loops and conditional statements inside the blade file.

## Views: Introduction
<!-- language-all: php -->

Views, in an MVC pattern, contain the logic on _how_ to present data to the user. In a web application, typically they are used to generate the HTML output that is sent back to users with each response. By default, views in Laravel are stored in the `resources/views` directory.

A view can be called using the `view` helper function: 
    
    view(string $path, array $data = [])

The first parameter of the helper is the path to a view file, and the second parameter is an optional array of data to pass to the view.

Therefore, to call the `resources/views/example.php`, you would use:

    view('example'); 

View files in subfolders within the `resources/views` directory, such as `resources/views/parts/header/navigation.php`, can be called using dot notation: `view('parts.header.navigation');`

Within a view file, such as `resources/views/example.php`, you're free to include both HTML and PHP together:

```
<html>
    <head>
        <title>Hello world!</title>
    </head>
    <body>
        <h1>Welcome!</h1>
        <p>Your name is: <?php echo $name; ?></p>
    </body>
</html>
```

In the previous example (which doesn't use any Blade specific syntax), we output the `$name` variable. To pass this value to our view, we would pass an array of values when calling the view helper: 

    view('example', ['name' => $name]);

or alternatively, use the `compact()` helper. In this case, the string passed to compact() corresponds to the name of the variable we want to pass to the view.

    view('example', compact('name'));

**NAMING CONVENTION FOR BLADE VARIABLES**

While sending data back to view. You can use `underscore` for multi-words `variable`but with `-` laravel gives error.

Like this one will give error (notice `hyphen ( - )` within the `user-address`

    view('example',['user-address' => 'Some Address']);

The **correct way** of doing this will be

    view('example', ['user_address' => 'Some Address']);

## Layout Inheritance
A layout is a view file, which is extended by other views which inject blocks of code into their parent. For example:

**parent.blade.php:**
<!-- language-all: php -->

    <html>
        <head>
            <style type='text/css'>
            @yield('styling')
            </style>
        </head>
        <body>
            <div class='main'>
            @yield('main-content')
            </div>
        </body>
    </html>

**child.blade.php:**

    @extends('parent')

    @section('styling')
    .main {
        color: red;
    }
    @stop
    
    @section('main-content')
    This is child page!
    @stop

**otherpage.blade.php:**

    @extends('parent')

    @section('styling')
    .main {
        color: blue;
    }
    @stop
    
    @section('main-content')
    This is another page!
    @stop

Here you see two example child pages, which each extend the parent. The child pages define a `@section`, which is inserted in the parent at the appropriate `@yield` statement.

So the view rendered by `View::make('child')` will say "**This is child page!**" in red, while `View::make('otherpage')` will produce the same html, except with the text "**This is another page!**" in blue instead.

It is common to separate the view files, e.g. having a layouts folder specifically for the layout files, and a separate folder for the various specific individual views.

The layouts are intended to apply code that should appear on every page, e.g. adding a sidebar or header, without having to write out all the html boilerplate in every individual view.

Views can be extended repeatedly - i.e. **page3** can `@extend('page2')`, and **page2** can `@extend('page1')`.

The extend command uses the same syntax as used for `View::make` and `@include`, so the file `layouts/main/page.blade.php` is accessed as `layouts.main.page`.

## Control Structures
Blade provides convenient syntax for common PHP control structures.

Each of the control structures begins with `@[structure]` and ends with `@[endstructure]`. Notice that within the tags, we are just typing normal HTML and including variables with the Blade syntax.

# Conditionals

## 'If' statements
<!-- language-all: php -->

    @if ($i > 10)
        <p>{{ $i }} is large.</p>
    @elseif ($i == 10)
        <p>{{ $i }} is ten.</p>
    @else
        <p>{{ $i }} is small.</p>
    @endif

## 'Unless' statements
(Short syntax for 'if not'.)

    @unless ($user->hasName())
        <p>A user has no name.</p>
    @endunless

# Loops

## 'While' loop

    @while (true)
        <p>I'm looping forever.</p>
    @endwhile

## 'Foreach' loop

    @foreach ($users as $id => $name)
        <p>User {{ $name }} has ID {{ $id }}.</p>
    @endforeach

## 'Forelse' Loop

(Same as 'foreach' loop, but adds a special `@empty` directive, which is executed when the array expression iterated over is empty, as a way to show default content .)

    @forelse($posts as $post)
        <p>{{ $post }} is the post content.</p>
    @empty
        <p>There are no posts.</p>
    @endforelse


----------


Within loops, a special `$loop` variable will be available, containing information about the state of the loop:


| Property           | Description
| ------------------ | ---
| `$loop->index`     | The index of the current loop iteration (starts at 0).
| `$loop->iteration` | The current loop iteration (starts at 1).
| `$loop->remaining` | The remaining loop iterations.
| `$loop->count`     | The total number of items in the array being iterated.
| `$loop->first`     | Whether this is the first iteration through the loop.
| `$loop->last`      | Whether this is the last iteration through the loop.
| `$loop->depth`     | The nesting level of the current loop.
| `$loop->parent`    | When in a nested loop, the parent's loop variable.

Example:

    @foreach ($users as $user)
      @foreach ($user->posts as $post)
            @if ($loop->parent->first)
                This is first iteration of the parent loop.
            @endif
        @endforeach
    @endforeach

----------

Since Laravel 5.2.22, we can also use the directives `@continue` and `@break`

| Property           | Description
| ------------------ | ---
| `@continue`        | Stop the current iteration and start the next one.
| `@break`           | Stop the current loop.

Example : 

    @foreach ($users as $user)
        @continue ($user->id == 2)
            <p>{{ $user->id }} {{ $user->name }}</p>
        @break ($user->id == 4)
    @endforeach

Then (_assuming 5+ users are sorted by ID and no ID is missing_) the page will render 

    1 Dave
    3 John
    4 William

## Echoing PHP expressions
Any PHP expression within double curly braces `{{ $variable }}` will be `echo`ed after being run through the [`e` helper function][1]. (So html special characters (`<`, `>`, `"`, `'`, `&`) are safely replaced for the corresponding html entities.)
(The PHP expression must evaluate to string, otherwise an exception will be thrown.)


## Echoing a variable

    {{ $variable }}

## Echoing an element in an array

    {{ $array["key"] }}

## Echoing an object property

    {{ $object->property }}

## Echoing the result of a function call

    {{ strtolower($variable) }}


# Checking for Existence

Normally, in PHP, to check if a variable is set and print it you would do

   - Before PHP 7

    <?php echo isset($variable) ? $variable : 'Default'; ?>
    
   - After PHP 7 (using the "Null coalescing operator" )

    <?php echo $variable ?? 'Default'; ?>

Blade operator `or` makes this easier:

    {{ $variable or 'Default' }}


# Raw echos

As mentioned, regular double braces syntax `{{ }}`, are filtered through PHP's `htmlspecialchars` function, for security (preventing malicious injection of HTML in the view). If you would like to bypass this behavior, for example if you're trying to output a block of HTML content resulting from a PHP expression, use the following syntax:

    {!! $myHtmlString !!}

Note that it is considered a best practice to use the standard `{{ }}` syntax to escape your data, unless absolutely necessary. In addition, when echoing untrusted content (ie. content supplied by users of your site), you should avoid using the `{!! !!}` syntax.


  [1]: https://laravel.com/docs/5.3/helpers#method-e

## Including Partial Views
With Blade, you can also include partial views (called 'partials') directly into a page like so:

    @include('includes.info', ['title' => 'Information Station'])

The code above will include the view at 'views/includes/info.blade.php'. It will also pass in a variable `$title` having value 'Information Station'.

In general, an included page will have access to any variable that the calling page has access to. For instance, if we have:

    {{$user}} // Outputs 'abc123'
    @include('includes.info')

And 'includes/info.blade.php' has the following:

    <p>{{$user}} is the current user.</p>

Then the page will render:

    abc123
    abc123 is the current user.

**Include Each**

Sometimes, you will want to combine an `include` statement with a `foreach` statement, and access the variables from within the foreach loop in the include. In this case, use Blade's `@each` directive:

    @each('includes.job', $jobs, 'job')

The first parameter is the page to include. The second parameter is the array to iterate over. The third parameter is the variable assigned to the elements of the array. The statement above is equivalent to:

    @foreach($jobs as $job)
        @include('includes.job', ['job' => $job])
    @endforeach

You can also pass an optional fourth argument to the `@each` directive to specify the view to show when the array is empty.

    @each('includes.job', $jobs, 'job', 'includes.jobsEmpty')

## Sharing data to all views


## Execute arbitrary PHP code
Although it might not be proper to do such thing in a view if you intend to separate concerns strictly, the `php` Blade directive allows a way to execute PHP code, for instance, to set a variable:

    @php($varName = 'Enter content ')

(same as:)

    @php
        $varName = 'Enter content ';
    @endphp

later:

    {{ $varName }}

Result:

> Enter content



