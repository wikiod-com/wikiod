---
title: "The Request"
slug: "the-request"
draft: false
images: []
weight: 9997
type: docs
toc: true
---

Symfony's Request class is an object-oriented representation of the HTTP request. It contains information such as the URL, query string, uploaded files, cookies and other headers coming from the browser.

## Syntax
 - $request->getPathInfo(); // returns the path (local part of the URL) that is being requested (but without the query string). I.e. when visiting https://example.com/foo/bar?key=value, this will contain /foo/bar
 - $request->query->get('id'); // returns a query string parameter ($_GET)
 - $request->query->get('id', 1); // returns a query string parameter with a default value
 - $request->request->get('name'); // returns a request body variable ($_POST)
 - $request->files->get('attachment'); // returns an instance of UploadedFile identified by "attachment"
 - $request->cookies->get('PHPSESSID'); // returns the value of a cookie ($_COOKIE)
 - $request->headers->get('content_type'); // returns an HTTP request header
 - $request->getMethod(); // returns the HTTP request method (GET, POST, PUT, DELETE, etc.)
 - $request->getLanguages(); // returns an array of languages the client accepts

## Getting a query string parameter
Lets say we want to build a paginated list of products, where the number of the page is passed as a query string parameter. For instance, to fetch the 3rd page, you'd go to:

    http://example.com/products?page=3

The raw HTTP request would look something like this:

    GET /products?page=3 HTTP/1.1
    Host: example.com
    Accept: text/html
    User-Agent: Mozilla/5.0 (Macintosh)

In order to get the page number from the request object, you can access the `query` property:

    $page = $request->query->get('page'); // 3

In the case of a `page` parameter, you will probably want to pass a default value in case the query string parameter is not set:

    $page = $request->query->get('page', 1);

This means that when someone visits http://example.com/products (note the absence of the query string), the `$page` variable will contain the default value `1`.

## Creating a Request object from global variables
PHP exposes a number of so-called *global variables* which contain information about the HTTP request, such as [`$_POST`][1], [`$_GET`][2], [`$_FILES`][3], [`$_SESSION`][4], etc. The `Request` class contains a static `createFromGlobals()` method in order to instantiate a request object based on these variables:

    use Symfony\Component\HttpFoundation\Request;

    $request = Request::createFromGlobals();

When using the Symfony framework, you should not instantiate the request object yourself. Instead, you should use the object that is instantiated when the framework is bootstrapped in `app.php` / `app_dev.php`. For instance by [type hinting the request object in your controller][5].


  [1]: https://secure.php.net/manual/en/reserved.variables.post.php
  [2]: https://secure.php.net/manual/en/reserved.variables.get.php
  [3]: https://secure.php.net/manual/en/reserved.variables.files.php
  [4]: https://secure.php.net/manual/en/reserved.variables.session.php
  [5]: https://www.wikiod.com/symfony/controllers#Using data from the Request object

## Accessing a POST variable
To get the contents of a form that is submitted with `method="post"`, use the `post` property:

    $name = $request->request->get('name');

## Accessing the contents of a cookie
    $id = $request->cookies->get('PHPSESSID');

This will return the value of the 'PHPSESSID' cookie sent by the browser.

