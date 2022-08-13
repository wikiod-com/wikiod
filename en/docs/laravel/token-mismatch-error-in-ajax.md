---
title: "Token Mismatch Error in AJAX"
slug: "token-mismatch-error-in-ajax"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

I have analyzed that ratio of getting TokenMismatch Error is very high. And this error occurs because of some silly mistakes. There are many reasons where developers are making mistakes. Here are some of the examples i.e No _token on headers, No _token passed data when using Ajax, permission issue on storage path, an invalid session storage path.

## Setup Token on Header
Set the token on `<head>` of your `default.blade.php`.

    <meta name="csrf-token" content="{{csrf_token()}}">

Add `ajaxSetup` on the top of your script, that will be accessible to everywhere. This will set headers on each `ajax` call

    $.ajaxSetup({
        headers: {
            'X-CSRF-TOKEN': $('meta[name="csrf-token"]').attr('content')
        }
    });

## Set token on <form> tag
Add below function to your `<form>` tag. This function will generate a hidden field named `_token` and filled value with the token.

    {{csrf_field()}}

Add `csrf_token ()` function to your hidden `_token` in the value attribute. This will generate only encrypted string.

`<input type="hidden" name="_token" value="{{csrf_token()}}"/>`.

## Check session storage path & permission
Here I assume that project app url is `APP_URL=http://project.dev/ts/toys-store`
1. Set the writable permission to `storage_path('framework/sessions')` the folder.
2. Check the path of your laravel project `'path' => '/ts/toys-store',` the root of your laravel project.
3. Change the name of your cookie `'cookie' => 'toys-store',`


    return [
        'driver' => env('SESSION_DRIVER', 'file'),
        'lifetime' => 120,
        'expire_on_close' => false,
        'encrypt' => false,
        'files' => storage_path('framework/sessions'),
        'connection' => null,
        'table' => 'sessions',
        'lottery' => [2, 100],
        'cookie' => 'toys-store',
        'path' => '/ts/toys-store',
        'domain' => null,
        'secure' => false,
        'http_only' => true,
    ];

## Use _token field on Ajax
There are many ways to send `_token` on AJAX call

1. Get all input field's value within  `<form>` tag using `var formData = new FormData($("#cart-add")[0]);`
2. Use `$("form").serialize();` or `$("form").serializeArray();`
3. Add `_token` manually on `data` of Ajax. using `$('meta[name="csrf-token"]').attr('content')` or `$('input[name="_token"]').val()`.
4. We can set as header on a particular Ajax call like below code.


    $.ajax({
        url: $("#category-add").attr("action"),
        type: "POST",
        data: formData,
        processData: false,
        contentType: false,
        dataType: "json",
        headers: {
            'X-CSRF-TOKEN': $('meta[name="csrf-token"]').attr('content')
        }
    });

