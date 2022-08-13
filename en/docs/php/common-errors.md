---
title: "Common Errors"
slug: "common-errors"
draft: false
images: []
weight: 9976
type: docs
toc: true
---

## Call fetch_assoc on boolean
If you get an error like this:

    Fatal error: Call to a member function fetch_assoc() on boolean in C:\xampp\htdocs\stack\index.php on line 7

Other variations include something along the lines of:

    mysql_fetch_assoc() expects parameter 1 to be resource, boolean given...

These errors mean that there is something wrong with either your query (this is a PHP/MySQL error), or your referencing. The above error was produced by the following code:

    $mysqli = new mysqli("localhost", "root", "");
        
    $query = "SELCT * FROM db"; // notice the errors here
    $result = $mysqli->query($query);
        
    $row = $result->fetch_assoc();

In order to "fix" this error, it is recommended to make mysql throw exceptions instead:

    // add this at the start of the script
    mysqli_report(MYSQLI_REPORT_ERROR | MYSQLI_REPORT_STRICT);

This will then throw an exception with this much more helpful message instead:

    You have an error in your SQL syntax; check the manual that corresponds to your MariaDB server version for the right syntax to use near 'SELCT * FROM db' at line 1

Another example that would produce a similar error, is where you simply just gave the wrong information to the `mysql_fetch_assoc` function or similar:

    $john = true;
    mysqli_fetch_assoc($john, $mysqli); // this makes no sense??

## Unexpected $end
    Parse error: syntax error, unexpected end of file in C:\xampp\htdocs\stack\index.php on line 4

If you get an error like this (or sometimes `unexpected $end`, depending on PHP version), you will need to make sure that you've matched up all inverted commas, all parentheses, all curly braces, all brackets, etc.

The following code produced the above error:

    <?php
    if (true) {
        echo "asdf";
    ?>

Notice the missing curly brace. Also do note that the line number shown for this error is irrelevant - it always shows the last line of your document.

