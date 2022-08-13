---
title: "The Admin Bar (aka The Toolbar)"
slug: "the-admin-bar-aka-the-toolbar"
draft: false
images: []
weight: 9975
type: docs
toc: true
---

The WordPress Admin Toolbar was added in version 3.1 and contains links to common administrative tasks as well as links to the user's profile and other WordPress information.  However, many site owners dislike showing the toolbar by default to all logged-in users and/or want to add their own options to it. 

## Removing the Admin Toolbar from all except Administrators
Add the following code to `functions.php` to remove it from everyone except the Administrator user level:

    add_action('after_setup_theme', 'no_admin_bar');
    
        function no_admin_bar() {
            if (!current_user_can('administrator') && !is_admin()) {
              show_admin_bar(false);
            }
        }

## Removing the Admin toolbar using filters
Another way to hide admin bar is to add

    if ( !current_user_can( 'manage_options' ) ) {
        add_filter( 'show_admin_bar', '__return_false' , 1000 );
    }

The users who don't have privileges to access Settings page, won't be able to see the admin bar.

## How to Remove WordPress Logo From Admin Bar
Developers can use **admin_bar_menu** action to remove items from WordPress admin bar or toolbar.

    add_action('admin_bar_menu', 'remove_wp_logo_from_admin_bar', 999);
    function remove_wp_logo_from_admin_bar( $wp_admin_bar ) {
        $wp_admin_bar->remove_node('wp-logo');
    }

Above code removes WordPress logo from the admin bar. All you need to do is paste the code inside your functions.php file.

The parameter passed to remove_node method is the ID of the node you wish to remove. ID's can be found in the HTML source code of WordPress page with a Toolbar on it. For example, the li element's ID for the WordPress Logo on the left in the Toolbar is "wp-admin-bar-wp-logo":

    <li id="wp-admin-bar-wp-logo" class="menupop"> … </li>

Remove "wp-admin-bar-" from the li's ID to get the ID of node. From this example the node ID is "wp-logo". 

You can use browser inspector tools to find out the node ID's of various items or nodes on your admin bar.

## Add your custom logo and custom link on admin login page
You can add below hooks to add your own logo and link to replace default wordpress logo.

**To add custom logo**

    function custom_login_logo() {
    echo '<style type="text/css">
    h1 a { background-image: url('.get_bloginfo('template_directory').'/images/custom-logo.png) !important; background-size : 100% !important; width: 300px !important; height : 100px !important;}
    </style>';
    }
    add_action('login_head', 'custom_login_logo');

**To add custom logo link**

    add_filter( 'login_headerurl', 'custom_loginlogo_url' );
    function custom_loginlogo_url($url) {
        return home_url();
    }

