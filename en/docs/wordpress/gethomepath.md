---
title: "get_home_path()"
slug: "get_home_path"
draft: false
images: []
weight: 9920
type: docs
toc: true
---

Get the absolute filesystem path to the root of the WordPress installation.

## Parameters
| Parameter | Details |
|----------|----------|
| *None*| This function does not accept any parameters. |

## Important difference between `get_home_path()` and `ABSTPATH`

Please keep in mind the difference between `ABSPATH` and `get_home_path()` if you have WordPress installed in a subfolder.

The `get_home_path()` function will always return a path **without** the subfolder:

- http://www.example.com - /var/www/htdocs/example
- http://www.example.com/wp - **/var/www/htdocs/example**

This is how it differs from `ABSPATH`, which will return different values:

- http://www.example.com - /var/www/htdocs/example
- http://www.example.com/wp - **/var/www/htdocs/example/wp**

`ABSPATH` is first defined in `wp-load.php` which will be located at `/var/www/htdocs/example/wp/wp-load.php` hence this is where `ABSPATH` will take its definition from. 

`get_home_path()` checks if the `site_url` and `home_url` differ, and removes the substring from the path. Otherwise it returns `ABSPATH` value:

    function get_home_path() {
        $home    = set_url_scheme( get_option( 'home' ), 'http' );
        $siteurl = set_url_scheme( get_option( 'siteurl' ), 'http' );
        if ( ! empty( $home ) && 0 !== strcasecmp( $home, $siteurl ) ) {
            $wp_path_rel_to_home = str_ireplace( $home, '', $siteurl ); /* $siteurl - $home */
            $pos = strripos( str_replace( '\\', '/', $_SERVER['SCRIPT_FILENAME'] ), trailingslashit( $wp_path_rel_to_home ) );
            $home_path = substr( $_SERVER['SCRIPT_FILENAME'], 0, $pos );
            $home_path = trailingslashit( $home_path );
        } else {
            $home_path = ABSPATH;
        }
 
        return str_replace( '\\', '/', $home_path );
    }

## Using it in your code

Calling `get_home_path()` must be done in a context where `wp-admin/includes/file.php` has already been included.

For example using `get_home_path()` within the `admin_init` hook is fine, but using it within the `init` is not and will result in a PHP fatal error:

    Call to undefined function get_home_path()

This file only gets included from within the admin (dashboard) context, if you absolutely need it outside of this context you will need to include the file yourself before calling the function:

    require_once(ABSPATH . 'wp-admin/includes/file.php');

## Usage
    $path = get_home_path();

**Return value:** 

`string` 

Full filesystem path to the root of the WordPress installation, even if it's installed in a subfolder.

**Example:**

`/var/www/htdocs/example`

