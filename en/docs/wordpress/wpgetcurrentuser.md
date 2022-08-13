---
title: "wp_get_current_user()"
slug: "wp_get_current_user"
draft: false
images: []
weight: 9965
type: docs
toc: true
---

## Get current loggedin user information
Retrieves the information pertaining to the currently logged in user, and places it in the global variable $current_user

This function does not accept any parameters.

Usage:

    <?php wp_get_current_user(); ?> 

Example:

      <?php
      $current_user = wp_get_current_user();

      echo 'Username: ' . $current_user->user_login . "\n";
      echo 'User email: ' . $current_user->user_email . "\n";
      echo 'User level: ' . $current_user->user_level . "\n";
      echo 'User first name: ' . $current_user->user_firstname . "\n";
      echo 'User last name: ' . $current_user->user_lastname . "\n";
      echo 'User display name: ' . $current_user->display_name . "\n";
      echo 'User ID: ' . $current_user->ID . "\n";
      ?>

To determine if a visitor is logged in or not, you can use is_user_logged_in() before, then get the current user info if the visitor it's logged in:

    <?php
    if ( is_user_logged_in() ) {
        $current_user = wp_get_current_user();

        echo 'Welcome, ' . $current_user->user_login . '!';
    } else {
        echo 'Welcome, visitor!';
    }
    ?>


