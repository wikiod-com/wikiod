---
title: "The $wpdb Object"
slug: "the-wpdb-object"
draft: false
images: []
weight: 9979
type: docs
toc: true
---

There are two ways to reference the `$wpdb` object. The first is to use the PHP keyword `global` in order to act on the global instance of the object.

    global $wpdb;
    echo $wpdb->prefix;
    // Outputs the prefix for the database

The second way to use the `$wpdb` object is to reference PHP's `$GLOBALS` super global variable.

    echo $GLOBALS['wpdb']->prefix;
    // This will also output the prefix for the database

The second way is discouraged as it may not be considered the best practice.

## Selecting a variable

In the most basic form, it is possible to select a single variable from a table by calling the object's `get_var` method passing in an SQL query.

    global $wpdb;
    $user = $wpdb->get_var( "SELECT ID FROM $wpdb->users WHERE user_email='foo@bar.com'" );

It is very important to note that any untrusted values used in queries *must* be escaped in order to protect against attacks. This can be done using the object's `prepare` method.

    global $wpdb;
    $email = $_POST['email'];
    $user = $wpdb->get_var(
        $wpdb->prepare( "SELECT ID FROM $wpdb->users WHERE user_email=%s", $email )
    );
    if( !is_null( $user ){
        echo $user; 
    } else {
        echo 'User not found';
    }


## Selecting multiple rows
You can use get_results to get multiple rows from database.

    global $wpdb;
    $userTable =$wpdb->prefix."users";
    
    $selectUser = $wpdb->get_results("SELECT * FROM $userTable"); 
This will show all users list in array.

