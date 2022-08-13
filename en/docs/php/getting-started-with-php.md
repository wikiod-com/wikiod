---
title: "Getting started with PHP"
slug: "getting-started-with-php"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## HTML output from web server
PHP can be used to add content to HTML files. While HTML is processed directly by a web browser, PHP scripts are executed by a web server and the resulting HTML is sent to the browser. 

The following HTML markup contains a PHP statement that will add `Hello World!` to the output:

    <!DOCTYPE html>
    <html>
        <head>
            <title>PHP!</title>
        </head>
        <body>
            <p><?php echo "Hello world!"; ?></p>
        </body>
    </html>

When this is saved as a PHP script and executed by a web server, the following HTML will be sent to the user's browser:

    <!DOCTYPE html>
    <html>
        <head>
            <title>PHP!</title>
        </head>
        <body>
            <p>Hello world!</p>
        </body>
    </html>

<!-- if version <PHP 5.x> [gte 5.4] -->

`echo` also has a shortcut syntax, which lets you immediately print a value. Prior to PHP 5.4.0, this short syntax only works with the [short_open_tag][1] configuration setting enabled. 

For example, consider the following code:

    <p><?= "Hello world!" ?></p>

Its output is identical to the output of the following:

    <p><?php echo "Hello world!"; ?></p>

<!-- end version if -->

In real-world applications, all data output by PHP to an HTML page should be properly *escaped* to prevent XSS ([Cross-site scripting][2]) attacks or text corruption.

See also: [Strings][3] and [PSR-1][4], which describes best practices, including the proper use of short tags (`<?= ... ?>`).


  [1]: http://php.net/manual/en/ini.core.php#ini.short-open-tag
  [2]: https://www.wikiod.com/php/security#Cross-Site Scripting (XSS)
  [3]: https://www.wikiod.com/php/types#Strings
  [4]: http://www.php-fig.org/psr/psr-1/

## Hello, World!
The most widely used language construct to print output in PHP is `echo`:

    echo "Hello, World!\n";

Alternatively, you can also use `print`:

    print "Hello, World!\n";

Both statements perform the same function, with minor differences:

- `echo` has a `void` return, whereas `print` returns an `int` with a value of `1`
- `echo` can take multiple arguments (without parentheses only), whereas `print` only takes one argument
- `echo` is [slightly faster][1] than `print`

Both `echo` and `print` are language constructs, not functions. That means they do not require parentheses around their arguments. For cosmetic consistency with functions, parentheses can be included. Extensive examples of the use of `echo` and `print` are [available elsewhere][3].

C-style `printf` and related functions are available as well, as in the following example:

    printf("%s\n", "Hello, World!");

See [Outputting the value of a variable][4] for a comprehensive introduction of outputting variables in PHP.


  [1]: http://www.phpbench.com/
  [3]: https://www.wikiod.com/php/outputting-the-value-of-a-variable#echo and print
  [4]: https://www.wikiod.com/php/outputting-the-value-of-a-variable

## Non-HTML output from web server
In some cases, when working with a web server, overriding the web server's default content type may be required. There may be cases where you need to send data as `plain text`, `JSON`, or `XML`, for example.  

The [`header()`][header] function can send a raw HTTP header. You can add the `Content-Type` header to notify the browser of the content we are sending.

Consider the following code, where we set `Content-Type` as `text/plain`:

    header("Content-Type: text/plain");
    echo "Hello World";

This will produce a plain text document with the following content:

> Hello World

To produce [JSON][1] content, use the `application/json` content type instead:

    header("Content-Type: application/json");
    
    // Create a PHP data array.
    $data = ["response" => "Hello World"];
    
    // json_encode will convert it to a valid JSON string.
    echo json_encode($data);

This will produce a document of type `application/json` with the following content:

> {"response":"Hello World"}

Note that the `header()` function must be called before PHP produces any output, or the web server will have already sent headers for the response. So, consider the following code:

    // Error: We cannot send any output before the headers
    echo "Hello";
    
    // All headers must be sent before ANY PHP output
    header("Content-Type: text/plain");
    echo "World";

This will produce a warning:

> **Warning:** Cannot modify header information - headers already sent by (output started at /dir/example.php:2) in **/dir/example.php** on line **3**

When using `header()`, its output needs to be the first byte that's sent from the server. For this reason it's important to not have empty lines or spaces in the beginning of the file before the PHP opening tag `<?php`. For the same reason, it is considered best practice (see [PSR-2][2]) to omit the PHP closing tag `?>` from files that contain only PHP and from blocks of PHP code at the very end of a file.

View the [**output buffering section**][output-buffering] to learn how to 'catch' your content into a variable to output later, for example, after outputting headers.

  [output-buffering]: https://www.wikiod.com/php/output-buffering
  [header]: http://php.net/manual/en/function.header.php


  [1]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/JSON/parse
  [2]: http://www.php-fig.org/psr/psr-2/#2-2-files

## PHP built-in server
PHP 5.4+ comes with a built-in development server. It can be used to run applications without having to install a production HTTP server such as nginx or Apache. The built-in server is only designed to be used for development and testing purposes.

It can be started by using the `-S` flag:

    php -S <host/ip>:<port>


# Example usage

 1. Create an `index.php` file containing:


    <?php
    echo "Hello World from built-in PHP server";

 2. Run the command `php -S localhost:8080` from the command line. Do not include `http://`. This will start a web server listening on port 8080 using the current directory that you are in as the document root.  

 3. Open the browser and navigate to `http://localhost:8080`. You should see your "Hello World" page.

# Configuration

To override the default document root (i.e. the current directory), use the `-t` flag:

    php -S <host/ip>:<port> -t <directory>

E.g. if you have a `public/` directory in your project you can serve your project from that directory using `php -S localhost:8080 -t public/`.

# Logs

Every time a request is made from the development server, a log entry like the one below is written to the command line.
    
    [Mon Aug 15 18:20:19 2016] ::1:52455 [200]: /

## PHP CLI
PHP can also be run from command line directly using the CLI (Command Line Interface).

CLI is basically the same as PHP from web servers, except some differences in terms of standard input and output.

# Triggering
The PHP CLI allows four ways to run PHP code:
1. Standard input. Run the `php` command without any arguments, but pipe PHP code into it: <pre>echo '&lt;?php echo "Hello world!";' | php</pre>
2. Filename as argument. Run the `php` command with the name of a PHP source file as the first argument: <pre>php hello_world.php</pre>
3. Code as argument. Use the `-r` option in the `php` command, followed by the code to run. The `<?php` open tags are not required, as everything in the argument is considered as PHP code: <pre>php -r 'echo "Hello world!";'</pre>
4. Interactive shell. Use the `-a` option in the `php` command to launch an interactive shell. Then, type (or paste) PHP code and hit <kbd>return</kbd>: <pre>$ php -a
Interactive mode enabled
php > echo "Hello world!";
Hello world!</pre>

# Output
All functions or controls that produce HTML output in web server PHP can be used to produce output in the stdout stream (file descriptor 1), and all actions that produce output in error logs in web server PHP will produce output in the stderr stream (file descriptor 2).

#### **`Example.php`**
    <?php
    echo "Stdout 1\n";
    trigger_error("Stderr 2\n");
    print_r("Stdout 3\n");
    fwrite(STDERR, "Stderr 4\n");
    throw new RuntimeException("Stderr 5\n");
    ?>
    Stdout 6

#### **Shell command line**

    $ php Example.php 2>stderr.log >stdout.log;\
    > echo STDOUT; cat stdout.log; echo;\
    > echo STDERR; cat stderr.log\

    STDOUT
    Stdout 1
    Stdout 3
    
    STDERR
    Stderr 4
    PHP Notice:  Stderr 2
     in /Example.php on line 3
    PHP Fatal error:  Uncaught RuntimeException: Stderr 5
     in /Example.php:6
    Stack trace:
    #0 {main}
      thrown in /Example.php on line 6

# Input
See: [Command Line Interface (CLI)][4]

  [1]: http://php.net/argc
  [2]: http://php.net/argv
  [3]: http://php.net/getopt
  [4]: https://www.wikiod.com/php/command-line-interface-cli

## Instruction Separation
Just like most other C-style languages, each statement is terminated with a semicolon. Also, a closing tag is used to terminate the last line of code of the PHP block. 

If the last line of PHP code ends with a semicolon, the closing tag is optional if there is no code following that final line of code. For example, we can leave out the closing tag after `echo "No error";` in the following example:

    <?php echo "No error"; // no closing tag is needed as long as there is no code below

However, if there is any other code following your PHP code block, the closing tag is no longer optional:

    <?php echo "This will cause an error if you leave out the closing tag"; ?>
    <html>
        <body>
        </body>
    </html>

We can also leave out the semicolon of the last statement in a PHP code block if that code block has a closing tag:

    <?php echo "I hope this helps! :D";
    echo "No error" ?>      

It is generally recommended to always use a semicolon and use a closing tag for every PHP code block except the last PHP code block, if no more code follows that PHP code block.

So, your code should basically look like this:
    
    <?php
        echo "Here we use a semicolon!";
        echo "Here as well!";
        echo "Here as well!";
        echo "Here we use a semicolon and a closing tag because more code follows";
    ?>
    <p>Some HTML code goes here</p>
    <?php
        echo "Here we use a semicolon!";
        echo "Here as well!";
        echo "Here as well!";
        echo "Here we use a semicolon and a closing tag because more code follows";
    ?>
    <p>Some HTML code goes here</p>
    <?php
        echo "Here we use a semicolon!";
        echo "Here as well!";
        echo "Here as well!";
        echo "Here we use a semicolon but leave out the closing tag";

## PHP Tags
There are three kinds of tags to denote PHP blocks in a file. The PHP parser is looking for the opening and (if present) closing tags to delimit the code to interpret.

# Standard Tags

These tags are the standard method to embed PHP code in a file.

    <?php
        echo "Hello World";
    ?>

<!-- if version <PHP 5.x> [gte 5.4] -->

# Echo Tags

These tags are available in all PHP versions, and since PHP 5.4 are always enabled. In previous versions, echo tags could only be enabled in conjunction with short tags.

    <?= "Hello World" ?>

<!-- end version if -->

# Short Tags

You can disable or enable these tags with the option `short_open_tag`.

    <?
        echo "Hello World";
    ?>

Short tags:
* are disallowed in all major PHP [coding standards][1]
* are discouraged in [the official documentation][2]
* are disabled by default in most distributions
* interfere with inline XML's processing instructions
* are not accepted in code submissions by most open source projects


<!-- if version <PHP 5.x> [lte 5.6] -->

# ASP Tags
By enabling the `asp_tags` option, ASP-style tags can be used.

    <%
        echo "Hello World";
    %>

These are an historic quirk and should never be used. They were removed in PHP 7.0.
<!-- end version if -->

  [1]: http://www.php-fig.org/psr/psr-1/
  [2]: https://secure.php.net/manual/en/language.basic-syntax.phptags.php

