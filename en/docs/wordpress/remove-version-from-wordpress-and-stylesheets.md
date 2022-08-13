---
title: "Remove Version from Wordpress and Stylesheets"
slug: "remove-version-from-wordpress-and-stylesheets"
draft: false
images: []
weight: 9981
type: docs
toc: true
---

To make it more difficult for others to hack your website you can remove the WordPress version number from your site, your css and js.
Without that number it's not possible to see if you run not the current version to exploit bugs from the older versions.

Additionally it can improve the loading speed of your site, because without query strings in the URL the css and js files can be cached.

## Syntax
- add_filter( $tag, $function_to_add, $priority, $accepted_args)

## Parameters
| Parameter | Details |
| ------ | ------ |
| $tag   | *(string required)*  Name of the filter where $function_to_add is hooked to |
| $function_to_add   | *(callable required)*  Name of the function which runs when the filter is applied |
| $priority  | *(int optional)* place of $function_to_add between other functions in one action (default = 10) |
| $accepted_args  | *(int optional)* Number of parameters which $function_to_add accepts (default = 1)  |

Intended to improve site speed and safety.

## Remove version numbers from css/js
Just add this function to your functions.php. It will remove the version from all enqueued js and css files.

    function remove_cssjs_ver( $src ) {
       if( strpos( $src, '?ver=' ) )
       $src = remove_query_arg( 'ver', $src );
       return $src;
    }
    
    add_filter( 'style_loader_src', 'remove_cssjs_ver', 999 );
    add_filter( 'script_loader_src', 'remove_cssjs_ver', 999 );

## Remove version  numbers from WordPress
If you add this to your functions.php it removes the WordPress version number from the RSS feed and the header.

    function remove_wordpress_ver() {
         return '';
    }
    add_filter('the_generator', 'remove_wordpress_ver', 999);

