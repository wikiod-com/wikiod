---
title: "Connecting to the database"
slug: "connecting-to-the-database"
draft: false
images: []
weight: 9997
type: docs
toc: true
---

Connect to the database to change/read/update/delete data!

## Syntax
 1. mysqli_connect("server", "user", "password", "database name");

## Parameters
| Paramater | Description|
| ------ | ------ |
| 1 | Server host e.g. "localhost" or "127.0.0.1" |
| 2 | Server username e.g. "root" or "redstonecoder" |
| 3 | Server password e.g. "" or "l3g1t"
| 4 | Database name e.g. "website" or "forum" |

As I showed in the example, I saved the connection to a variable to make it cleaner in code.
Otherwise, when you want to query the database, you need to connect EACH time!

It makes the code a lot cleaner!

## mysqli_connect
    <?php
    
    $con = mysqli_connect("localhost", "root", "", "database");
    
    ?>

