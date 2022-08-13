---
title: "Cassandra - PHP"
slug: "cassandra---php"
draft: false
images: []
weight: 9979
type: docs
toc: true
---

## Simple console application
Download the Datastax PHP driver for Apache Cassandra from [the Git project site][1], and follow the installation instructions.

We will be using the **"users"** table from the **KillrVideo** app and the Datastax PHP driver. Once you have Cassandra up and running, create the following keyspace and table:

    //Create the keyspace
        CREATE KEYSPACE killrvideo WITH REPLICATION = 
        { 'class' : 'SimpleStrategy', 'replication_factor' : 1 };
    
    //Use the keyspace
    USE killrvideo;
    
    // Users keyed by id
    CREATE TABLE users (
        userid uuid,
        firstname text,
        lastname text,
        email text,
        created_date timestamp,
        PRIMARY KEY (userid)
    );

Create a PHP file with your favorite editor.  First, we need to connect to our **“killrvideo”** keyspace and cluster:

    build();
    $keyspace  = 'killrvideo';
    $session  = $cluster->connect($keyspace);

Let’s insert a user into the **“users”** table:

    execute(new Cassandra\SimpleStatement(
        "INSERT INTO users (userid, created_date, email, firstname, lastname) 
          VALUES (14c532ac-f5ae-479a-9d0a-36604732e01d, '2013-01-01 00:00:00', 
          'luke@example.com','Luke','Tillman')"
      ));

Using the Datastax PHP Cassandra driver, we can query the user by userid:

    execute(new Cassandra\SimpleStatement
              ("SELECT firstname, lastname, email FROM killrvideo.users
                WHERE userid=14c532ac-f5ae-479a-9d0a-36604732e01d"));
    
    foreach ($result as $row) {
        printf("user: \"%s\" \"%s\" email: \"%s\" \n", $row['firstname'],
        $row['lastname'], $row['email']);
      }

For a user to update their email address in the system:

    execute(new Cassandra\SimpleStatement
              ("UPDATE users SET email = 'language_evangelist@example.com' 
                WHERE userid = 14c532ac-f5ae-479a-9d0a-36604732e01d"));

    execute(new Cassandra\SimpleStatement
              ("SELECT firstname, lastname, email FROM killrvideo.users 
                WHERE userid=14c532ac-f5ae-479a-9d0a-36604732e01d"));

    foreach ($result as $row) {
        printf("user: \"%s\" \"%s\" email: \"%s\" \n", $row['firstname'], 
        $row['lastname'], $row['email']);
      }

Delete the user from the table and output all of the rows. You’ll notice that the user's information no longer comes back after being deleted:

    execute(new Cassandra\SimpleStatement
              ("DELETE FROM users WHERE userid = 14c532ac-f5ae-479a-9d0a-36604732e01d"));

    execute(new Cassandra\SimpleStatement
              ("SELECT firstname, lastname, email FROM killrvideo.users 
                WHERE userid=14c532ac-f5ae-479a-9d0a-36604732e01d"));

    foreach ($result as $row) {
        printf("user: \"%s\" \"%s\" email: \"%s\" \n", $row['firstname'], 
        $row['lastname'], $row['email']);
      }

The output should look something like this:

    user: "Luke" "Tillman" email: "luke@example.com" 
    user: "Luke" "Tillman" email: "language_evangelist@example.com"

**References**:

[Getting Started with Apache Cassandra and PHP][2], DataStax Academy


  [1]: https://github.com/datastax/php-driver
  [2]: https://academy.datastax.com/resources/getting-started-apache-cassandra-and-php

