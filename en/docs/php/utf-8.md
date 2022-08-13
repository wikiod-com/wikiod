---
title: "UTF-8"
slug: "utf-8"
draft: false
images: []
weight: 9934
type: docs
toc: true
---

- You need to make sure that every time you process a UTF-8 string, you do so safely.  This is, unfortunately, the hard part.  You'll probably want to make extensive use of PHP's [`mbstring`][1] extension.

- **PHP's built-in string operations are *not* by default UTF-8 safe.**  There are some things you can safely do with normal PHP string operations (like concatenation), but for most things you should use the equivalent [`mbstring`][1] function.



[1]: http://www.php.net/manual/en/book.mbstring.php

## Input
- You should verify every received string as being valid UTF-8 before you try to store it or use it anywhere. PHP's [`mb_check_encoding()`][1] does the trick, but you have to use it consistently. There's really no way around this, as malicious clients can submit data in whatever encoding they want.

      $string = $_REQUEST['user_comment'];
      if (!mb_check_encoding($string, 'UTF-8')) {
          // the string is not UTF-8, so re-encode it.
          $actualEncoding = mb_detect_encoding($string);
          $string = mb_convert_encoding($string, 'UTF-8', $actualEncoding);
      }

- **If you're using HTML5 then you can ignore this last point.** You want all data sent to you by browsers to be in UTF-8. The only reliable way to do this is to add the `accept-charset` attribute to all of your `<form>` tags like so:

      <form action="somepage.php" accept-charset="UTF-8">



[1]: http://php.net/manual/en/function.mb-check-encoding.php
[2]: http://whatwg.org/html


## Output
- If your application transmits text to other systems, they will also need to be informed of the character encoding. In PHP, you can use the [`default_charset`][2] option in `php.ini`, or manually issue the `Content-Type` MIME header yourself. This is the preferred method when targeting modern browsers.

      header('Content-Type: text/html; charset=utf-8');

- If you are unable to set the response headers, then you can also set the encoding in an HTML document with [HTML metadata][1].
    - HTML5

          <meta charset="utf-8">

    - Older versions of HTML

          <meta http-equiv="Content-Type" content="text/html; charset=utf-8" />



[1]: http://stackoverflow.com/q/4696499/4245525
[2]: http://www.php.net/manual/en/ini.core.php#ini.default-charset


## Data Storage and Access
> This topic specifically talks about UTF-8 and considerations for using it with a database. If you want more information about using databases in PHP then checkout [this topic][8].

**Storing Data in a MySQL Database:**

- Specify the `utf8mb4` character set on all tables and text columns in your database.  This makes MySQL physically store and retrieve values encoded natively in UTF-8.
  >MySQL will implicitly use `utf8mb4` encoding if a `utf8mb4_*` collation is specified (without any explicit character set).
- Older versions of MySQL (< 5.5.3) do not support `utf8mb4` so you'll be forced to use `utf8`, which only supports a subset of Unicode characters.

**Accessing Data in a MySQL Database:**

- In your application code (e.g. PHP), in whatever DB access method you use, you'll need to set the connection charset to `utf8mb4`.  This way, MySQL does no conversion from its native UTF-8 when it hands data off to your application and vice versa.
- Some drivers provide their own mechanism for configuring the connection character set, which both updates its own internal state and informs MySQL of the encoding to be used on the connection. This is usually the preferred approach.  

  For Example (The same consideration regarding `utf8mb4`/`utf8` applies as above):

    - If you're using the [PDO][1] abstraction layer with PHP &ge; 5.3.6, you can specify `charset` in the [DSN][2]:

          $handle = new PDO('mysql:charset=utf8mb4');

    - If you're using [mysqli][3], you can call [`set_charset()`][4]:

          $conn = mysqli_connect('localhost', 'my_user', 'my_password', 'my_db');

          $conn->set_charset('utf8mb4');        // object oriented style
          mysqli_set_charset($conn, 'utf8mb4'); // procedural style

    - If you're stuck with plain [mysql][5] but happen to be running PHP &ge; 5.2.3, you can call [`mysql_set_charset`][6].

          $conn = mysql_connect('localhost', 'my_user', 'my_password');

          $conn->set_charset('utf8mb4');       // object oriented style
          mysql_set_charset($conn, 'utf8mb4'); // procedural style
    
    - If the database driver does not provide its own mechanism for setting the connection character set, you may have to issue a query to tell MySQL how your application expects data on the connection to be encoded: [`SET NAMES 'utf8mb4'`][7].




[1]: http://www.php.net/manual/en/book.pdo.php
[2]: http://www.php.net/manual/en/ref.pdo-mysql.connection.php
[3]: http://www.php.net/manual/en/book.mysqli.php
[4]: http://www.php.net/manual/en/mysqli.set-charset.php
[5]: http://www.php.net/manual/en/book.mysql.php
[6]: http://www.php.net/manual/en/function.mysql-set-charset.php
[7]: http://dev.mysql.com/doc/en/charset-connection.html
[8]: https://www.wikiod.com/php
 

