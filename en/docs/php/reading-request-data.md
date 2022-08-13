---
title: "Reading Request Data"
slug: "reading-request-data"
draft: false
images: []
weight: 9937
type: docs
toc: true
---

## Choosing between GET and POST ##

**GET** requests, are best for providing data that's needed to render the page and may be used multiple times (search queries, data filters...). They are a part of the URL, meaning that they can be bookmarked and are often reused.

**POST** requests on the other hand, are meant for submitting data to the server just once (contact forms, login forms...). Unlike GET, which only accepts ASCII, POST requests also allow binary data, including [file uploads][1].

You can find a more detailed explanation of their differences [here][2].

## Request Data Vulnerabilities ##

Also look at: http://stackoverflow.com/questions/1301863/what-are-the-vulnerabilities-in-direct-use-of-get-and-post

Retrieving data from the $_GET and $_POST superglobals without any validation is considered bad practice, and opens up methods for users to potentially access or compromise data through [code][3] and or [SQL injections][4]. Invalid data should be checked for and rejected as to prevent such attacks.

Request data should be escaped depending on how it is being used in code, as noted [here][5] and [here][6]. A few different escape functions for common data use cases can be found in [this answer][7].

  [1]: https://www.wikiod.com/php
  [2]: http://www.w3schools.com/tags/ref_httpmethods.asp
  [3]: https://www.owasp.org/index.php/Code_Injection
  [4]: https://www.wikiod.com/php/security
  [5]: http://stackoverflow.com/a/130323/2104168
  [6]: http://stackoverflow.com/a/4224002/2104168
  [7]: http://stackoverflow.com/a/1206461/2104168

## Reading raw POST data
Usually data sent in a POST request is structured key/value pairs with a MIME type of `application/x-www-form-urlencoded`. However many applications such as web services require raw data, often in XML or JSON format, to be sent instead. This data can be read using one of two methods.

**`php://input`** is a stream that provides access to the raw request body.

    $rawdata = file_get_contents("php://input");
    // Let's say we got JSON
    $decoded = json_decode($rawdata);

<!-- if version [lt 5.6] -->
**`$HTTP_RAW_POST_DATA`** is a global variable that contains the raw POST data. It is only available if the `always_populate_raw_post_data` directive in `php.ini` is enabled.

    $rawdata = $HTTP_RAW_POST_DATA;
    // Or maybe we get XML
    $decoded = simplexml_load_string($rawdata);

This variable has been deprecated since PHP version 5.6, and was removed in PHP 7.0.
<!-- end version if -->

Note that neither of these methods are available when the content type is set to `multipart/form-data`, which is used for file uploads.

## Reading POST data
Data from a POST request is stored in the [superglobal][1] `$_POST` in the form of an associative array.

Note that accessing a non-existent array item generates a notice, so existence should always be checked with the `isset()` or `empty()` functions, or the null coalesce operator.

Example:

    $from = isset($_POST["name"]) ? $_POST["name"] : "NO NAME";
    $message = isset($_POST["message"]) ? $_POST["message"] : "NO MESSAGE";
    
    echo "Message from $from: $message";

<!-- if version [gte 7.0] -->
    $from = $_POST["name"] ?? "NO NAME";
    $message = $_POST["message"] ?? "NO MESSAGE";
    
    echo "Message from $from: $message";
<!-- end version if -->

  [1]: http://php.net/manual/en/language.variables.superglobals.php "PHP manual"

## Reading GET data
Data from a GET request is stored in the [superglobal][1] `$_GET` in the form of an associative array.

Note that accessing a non-existent array item generates a notice, so existence should always be checked with the `isset()` or `empty()` functions, or the null coalesce operator.

Example: (for URL `/topics.php?author=alice&topic=php`)

    $author = isset($_GET["author"]) ? $_GET["author"] : "NO AUTHOR";
    $topic = isset($_GET["topic"]) ? $_GET["topic"] : "NO TOPIC";
    
    echo "Showing posts from $author about $topic";

<!-- if version [gte 7.0] -->
    $author = $_GET["author"] ?? "NO AUTHOR";
    $topic = $_GET["topic"] ?? "NO TOPIC";
    
    echo "Showing posts from $author about $topic";

<!-- end version if -->

  [1]: http://php.net/manual/en/language.variables.superglobals.php "PHP manual"

## Handling file upload errors


## Uploading files with HTTP PUT


## Passing arrays by POST
Usually, an HTML form element submitted to PHP results in a single value. For example:

    <pre>
    <?php print_r($_POST);?>
    </pre>
    <form method="post">
        <input type="hidden" name="foo" value="bar"/>
        <button type="submit">Submit</button>
    </form>

This results in the following output:

    Array
    (
        [foo] => bar
    )

However, there may be cases where you want to pass an array of values. This can be done by adding a PHP-like suffix to the name of the HTML elements:

    <pre>
    <?php print_r($_POST);?>
    </pre>
    <form method="post">
        <input type="hidden" name="foo[]" value="bar"/>
        <input type="hidden" name="foo[]" value="baz"/>
        <button type="submit">Submit</button>
    </form>

This results in the following output:

    Array
    (
        [foo] => Array
            (
                [0] => bar
                [1] => baz
            )
    
    )

You can also specify the array indices, as either numbers or strings:

    <pre>
    <?php print_r($_POST);?>
    </pre>
    <form method="post">
        <input type="hidden" name="foo[42]" value="bar"/>
        <input type="hidden" name="foo[foo]" value="baz"/>
        <button type="submit">Submit</button>
    </form>

Which returns this output:

    Array
    (
        [foo] => Array
            (
                [42] => bar
                [foo] => baz
            )
    
    )

This technique can be used to avoid post-processing loops over the `$_POST` array, making your code leaner and more concise.

