---
title: "Getting started with mysqli"
slug: "getting-started-with-mysqli"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation or Setup
MySQLi is a PHP Extension which enables PHP to communicate with MySQL Databases. **MySQLi comes built in with PHP**. MySQLi was introduced with PHP 5.0.

For More details about Installations Refer official [PHP MySQLi Documentation][1]


  [1]: http://php.net/manual/en/mysqli.installation.php

## MySQLi Basic Example in Procedural Style
    <?php 
    //Creating Connection to MySQL database using MySQLi
    $mysqli = mysqli_connect("IP ADDRESS OR DOAMIN", "username", "password", "database_name");

    //Executing Query in the Database using MySQLi
    $result = mysqli_query($mysqli, "SELECT * FROM TABLE");

    //Getting Results from MySQL 
    $row = mysqli_fetch_assoc($result);
    echo $row[columnName];

    //Closing connection to the Database
    $mysqli_close($mysqli);

    ?>


