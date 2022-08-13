---
title: "Options API"
slug: "options-api"
draft: false
images: []
weight: 9988
type: docs
toc: true
---

Options are pieces of data that WordPress uses to store various preferences and configuration settings. The Options API is a simple and standardized way of storing data in the database. The API makes it easy to create, access, update, and delete options.

## Syntax
-  // Create new option within WordPress  
add_option( $option, $value = , $deprecated = , $autoload = 'yes' );
- // Removes an option from the database.  
delete_option( $option );
- // Retrieve a saved option  
get_option( $option, $default = false );
- // Update the value of an option that was already added.  
update_option( $option, $newvalue );

- // There are also *_site_option() versions of these functions,  
// to manipulate network-wide options in WordPress Multisite

- // Create new network option  
add_site_option( $option, $value = , $deprecated = , $autoload = 'yes' );
- // Removes a network option  
delete_site_option( $option );
- // Retrieve a saved network option  
get_site_option( $option, $default = false );
- // Update the value of an option that was already added.  
update_site_option( $option, $newvalue );

The Options API is a simple and standardized way of working with data staored in the options table of MySQL database. The API makes it easy to create, read, update, and delete options.

## get_option
get_option function is used to retrieve a value from from options table based on option name.

You can use the following code to get email address of a WordPress site administrator.

    <?php echo get_option('admin_email'); ?>

`get_option()` has an optional 2nd argument, which allows you to set a default value to return in the case that the requested option isn't set. By default, this argument is `false`.

To retrieve a text string, and use a boilerplate string if the text isn't set in the options table, you could do this:

    <?php get_option( 'my_text', "I don't have anything written. Yet." ); ?>



## add_option
add_option function ins used to insert new row into options table.

This will insert a new row in options table with option name *some_option_name* and value as *some_option_value*

    <?php add_option( 'some_option_name', 'some_option_value' ); ?>

## delete_option
delete_option function is used to delete an option from the options table.

This will delete *my_custom_option* from the options table.

    <?php delete_option( 'my_custom_option' ); ?>

## update_option
update_option function is used to update a value that already exists in the options table. If the option does not exist, then the option will be added with the option value.

This will set the default comment status to 'closed':

    update_option( 'default_comment_status', 'closed' );

