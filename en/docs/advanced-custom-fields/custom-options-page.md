---
title: "Custom Options page"
slug: "custom-options-page"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

## Syntax
 - acf_add_options_page( $args );
 - acf_add_options_sub_page( $page );

## Parameters
| Parameter | Details |
| ------ | ------ |
| $args   | (mixed) A string for the page title, or an array of settings. If left blank, default settings will be used.   |
| $page   | (mixed) A string for the page title, or an array of settings. If left blank, default settings will be used.   |

## The simplest Options page
    if( function_exists('acf_add_options_page') ) {
        acf_add_options_page();
    }

Add the above code to `functions.php` and an options page named 'Options' will appear in your Wordpress admin area. You now need to asign some custom fields to the page.

## Advanced Options page
    if( function_exists('acf_add_options_page') ) {
        
        acf_add_options_page(array(
            'page_title'     => 'Theme General Settings',
            'menu_title'    => 'Theme Settings',
            'menu_slug'     => 'theme-general-settings',
            'capability'    => 'edit_posts',
            'redirect'        => false
        ));
        
        acf_add_options_sub_page(array(
            'page_title'     => 'Theme Header Settings',
            'menu_title'    => 'Header',
            'parent_slug'    => 'theme-general-settings',
        ));
        
    }

Add the above code to `functions.php` and an options page named 'Theme Settings' will appear in your Wordpress admin area. So will a sub page named 'Header'.

You now need to asign some custom fields to those pages.

**All supported arguments of both functions:**

    $args [or $page] = array(
        
        /* (string) The title displayed on the options page. Required. */
        'page_title' => 'Options',
        
        /* (string) The title displayed in the wp-admin sidebar. Defaults to page_title */
        'menu_title' => '',
        
        /* (string) The slug name to refer to this menu by (should be unique for this menu). 
        Defaults to a url friendly version of menu_slug */
        'menu_slug' => '',
        
        /* (string) The capability required for this menu to be displayed to the user. Defaults to edit_posts.
        Read more about capability here: http://codex.wordpress.org/Roles_and_Capabilities */
        'capability' => 'edit_posts',
        
        /* (int|string) The position in the menu order this menu should appear. 
        WARNING: if two menu items use the same position attribute, one of the items may be overwritten so that only one item displays!
        Risk of conflict can be reduced by using decimal instead of integer values, e.g. '63.3' instead of 63 (must use quotes).
        Defaults to bottom of utility menu items */
        'position' => false,
        
        /* (string) The slug of another WP admin page. if set, this will become a child page. */
        'parent_slug' => '',
        
        /* (string) The icon class for this menu. Defaults to default WordPress gear.
        Read more about dashicons here: https://developer.wordpress.org/resource/dashicons/ */
        'icon_url' => false,
        
        /* (boolean) If set to true, this options page will redirect to the first child page (if a child page exists). 
        If set to false, this parent page will appear alongside any child pages. Defaults to true */
        'redirect' => true,
        
        /* (int|string) The '$post_id' to save/load data to/from. Can be set to a numeric post ID (123), or a string ('user_2'). 
        Defaults to 'options'. Added in v5.2.7 */
        'post_id' => 'options',
        
        /* (boolean)  Whether to load the option (values saved from this options page) when WordPress starts up. 
        Defaults to false. Added in v5.2.8. */
        'autoload' => false,
        
    );

