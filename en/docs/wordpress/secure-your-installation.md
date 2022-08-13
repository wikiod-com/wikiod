---
title: "Secure your installation"
slug: "secure-your-installation"
draft: false
images: []
weight: 9968
type: docs
toc: true
---

WordPress websites are frequently hacked. This topic is for techniques and practices that increase the security of your WordPress installation beyond what is achieved in a base install.

Apart from this topic, another good place to read about securing a WordPress installation is the [Hardening WordPress Codex page][1]. 

[1]: https://codex.wordpress.org/Hardening_WordPress

## Move wp-config.php
The most sensitive information of a WordPress install is stored in the `wp-config.php` file. If a hacker gets access to this file then they have total control of your website.

By default `wp-config.php` is stored in the WordPress install folder. To make this file harder to steal you can move it out of the web accessible folder. If you move it just one folder above, WordPress will automatically find it. If you move `wp-config.php` to a different location, create an empty file called `wp-config.php` in the WordPress installation folder. Then add the following:

    define('ABSPATH', dirname(__FILE__) . '/');
    // '../../wp-config.php' defines location two folders above installation folder.
    // Substitute with actual location of wp-config.php file as necessary.
    require_once(ABSPATH . '../../wp-config.php');

You may need to make `php` executable in the folder you place wp-config.php in. You should make php executable in as few folders as possible. A good system puts the WordPress install in `/path/to/wordpress/install/` and the config in `/path/to/wordpress/config`. You'd make sure the config folder is not web accessible and don't place any other sensitive information would be placed in `/path/to/` or higher in the folder hierarchy. In that case you'd write a line similar to the following in your `php.ini`:

    open_basedir = "/path/to/wordpress/install/;/path/to/wordpress/config"

This technique is controversial and some people don't think it enhances security. Extensive discussion on the topic can be read at [this WordPress StackExchange question][1].

[1]: http://wordpress.stackexchange.com/questions/58391/is-moving-wp-config-outside-the-web-root-really-beneficial

## Disable File Editor
The file editor that ships with WordPress is a security risk. If an attacker gains admin access to your WordPress website they will be easily able to insert malicious code into theme and plugin files. It is also a risk with clients who don't know what they're doing. Once misplaced colon in the file editor can break a site and make it inaccessible from the browser.

In your WordPress `wp-config.php` file, disable the file editor by adding the following line of code.

    define( 'DISALLOW_FILE_EDIT', true );

That line will have the desired effect when added to your theme's `functions.php` file too but it is better to add to `wp-config.php`. 

If you are using [WordPress CLI][1] to install WordPress you can use the following command to create a `wp-config.php` file with file editing disabled.

    /* declare variables beforehand or substitute strings in */
    wp core config --dbname="$MYSQL_DBNAME" --dbuser="$MYSQL_USERNAME" --dbpass="$MYSQL_PASS" --dbprefix="$WP_DBPREFIX"_ --locale=en_AU --extra-php <<PHP
    define( 'DISALLOW_FILE_EDIT', true );
    PHP

This method is useful if you install WordPress with a script.

[1]: http://wp-cli.org/config/



## Set a custom prefix for WordPress tables
When you install WordPress to your server, the installation script will place a prefix in front of all the WordPress MySQL table names. This prefix is set to 'wp_' by default. The WordPress posts table will be called `wp_posts` for example. By changing the table prefix you can create some security by obscurity. This way when a hacker attempts SQL injection attacks, they will have to guess the prefix of your table rather than just using 'wp_'. You can set this prefix to be whatever you like. 

**Set Prefix in New WordPress Installation**

If using famous 5 minute installation Change prefix in field during installation.

[![enter image description here][1]][1] 

If installing via WordPress CLI use the following command:

    // set other variables above, or substitute your strings in.
    WP_DBPREFIX=foo
    wp core config --dbname="$MYSQL_DBNAME" --dbuser="$MYSQL_USERNAME" --dbpass="$MYSQL_PASS" --dbprefix="$WP_DBPREFIX"_ --locale=en_AU

**Change Prefix in Existing Installation**

Changing the prefix is a little more difficult. Firstly use a FTP program like FileZilla to edit the `wp-config.php` file. Change the entry `$table_prefix = 'wp_';` to `   $table_prefix = 'foo_';` substituting 'foo' for your desired prefix. 

Next we'll need to edit the database. If you have access to phpMyAdmin, login and do the following:

 - Select the WordPress database [![enter image description here][2]][2]
 - Select all tables and in the dropdown select replace table prefix.[![enter image description here][3]][3]
 - In "From" type 'wp_'. In "To" type your prefix, 'foo_' in this example and press "Submit". [![enter image description here][4]][4]
 - Tables should now look like this:[![enter image description here][5]][5]

If you can't use phpMyAdmin then use the following MySQL command:

    RENAME table `wp_comments` TO `foo_comments`

You'll need to run that command for each table, substituting 'comments' for the other table names.

Next we need to change a few entries in some tables. Run this query on the 'foo_options' table

    SELECT * FROM  foo_options WHERE option_name LIKE '%user_roles%'

A entry with option_name of 'wp_user_roles' should appear. In that entry change the 'option_name' entry from `wp_user_roles` to `foo_user_roles`.

Then open up 'foo_usermeta' table and find every entry with 'wp_' at the front.
[![enter image description here][6]][6]
 and change it to 'foo_'. The number of entries you must change will depend on how many users you have.
[![enter image description here][7]][7]

That should be all you need to change the prefix in an existing installation

  [1]: https://i.stack.imgur.com/uoyrZ.png
  [2]: https://i.stack.imgur.com/7jZpM.png
  [3]: https://i.stack.imgur.com/EyEdI.png
  [4]: https://i.stack.imgur.com/8ZSju.png
  [5]: https://i.stack.imgur.com/yi74Q.png
  [6]: https://i.stack.imgur.com/yz9kF.png
  [7]: https://i.stack.imgur.com/1Qf5r.png

