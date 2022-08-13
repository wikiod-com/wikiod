---
title: "Connecting with UTF-8 Using Various Programming language."
slug: "connecting-with-utf-8-using-various-programming-language"
draft: false
images: []
weight: 9985
type: docs
toc: true
---

## Python
1st or 2nd line in source code (to have literals in the code utf8-encoded):

    # -*- coding: utf-8 -*-

Connection:

    db = MySQLdb.connect(host=DB_HOST, user=DB_USER, passwd=DB_PASS, db=DB_NAME,
            charset="utf8mb4", use_unicode=True)

For web pages, one of these:

    <meta charset="utf-8" />
    <meta http-equiv="Content-Type" content="text/html; charset=utf-8" />

## PHP
In php.ini (this is the default after PHP 5.6):

    default_charset UTF-8

When building a web page:

    header('Content-type: text/plain; charset=UTF-8');

When connecting to MySQL:

    (for mysql:)   Do not use the mysql_* API!
    (for mysqli:)  $mysqli_obj->set_charset('utf8mb4');
    (for PDO:)     $db = new PDO('dblib:host=host;dbname=db;charset=utf8', $user, $pwd);

In code, do not use any conversion routines.

For data entry,

    <form accept-charset="UTF-8">

For JSON, to avoid `\uxxxx`:

    $t = json_encode($s, JSON_UNESCAPED_UNICODE);

