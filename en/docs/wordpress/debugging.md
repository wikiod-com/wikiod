---
title: "Debugging"
slug: "debugging"
draft: false
images: []
weight: 9960
type: docs
toc: true
---

https://codex.wordpress.org/Debugging_in_WordPress

Debugging PHP code is part of any project, but WordPress comes with specific debug systems designed to simplify the process as well as standardize code across the core, plugins and themes.

Plugins for debugging in WordPress:

* https://wordpress.org/plugins/query-monitor/
* https://wordpress.org/plugins/debug-bar/
* https://wordpress.org/plugins/debug-bar-console/
* https://wordpress.org/plugins/kint-debugger/
* https://wordpress.org/plugins/rest-api-console/


## Example wp-config.php and good practices for Debugging
The following code, inserted in your wp-config.php file, will log all errors, notices, and warnings to a file called debug.log in the wp-content directory. It will also hide the errors so they do not interrupt page generation.

     // Enable WP_DEBUG mode
    define( 'WP_DEBUG', true );
    
    // Enable Debug logging to the /wp-content/debug.log file
    define( 'WP_DEBUG_LOG', true );
    
    // Disable display of errors and warnings 
    define( 'WP_DEBUG_DISPLAY', false );
    @ini_set( 'display_errors', 0 );
    
    // Use dev versions of core JS and CSS files (only needed if you are modifying these core files)
    define( 'SCRIPT_DEBUG', true );


Good practice If you want add custom messages to debug log add folowing code in your plugin or theme.

    //Checking is function_exists  
        if ( !function_exists( 'print_to_log' ) ) {
                //function writes a message to debug.log if debugging is turned on.
                function print_to_log( $message )
                {
                    if ( true === WP_DEBUG ) {
                        if ( is_array( $message ) || is_object( $message ) ) {
                            error_log( print_r( $message, true ) );
                        } else {
                            error_log( $message );
                        }
                    }
                }
            }

## SAVEQUERIES
The SAVEQUERIES definition saves the database queries to an array and that array can be displayed to help analyze those queries. The constant defined as true causes each query to be saved, how long that query took to execute, and what function called it.
NOTE: This will have a performance impact on your site, so make sure to turn this off when you aren't debugging.

    define( 'SAVEQUERIES', true );
The array is stored in the 

    global $wpdb->queries;

## WP_DEBUG_LOG
[`WP_DEBUG_LOG`](https://codex.wordpress.org/WP_DEBUG) is a companion to WP_DEBUG that causes all errors to also be saved to a debug.log log file inside the /wp-content/ directory. This is useful if you want to review all notices later or need to view notices generated off-screen (e.g. during an AJAX request or wp-cron run).
   
    //enable
    define( 'WP_DEBUG_LOG', true );

    //disable
    define( 'WP_DEBUG_LOG', false );



## See logs in a separate file
When you have an ajax call, it's extremely difficult to get a log from inside of the callback function. But if you enable the debugging

    define('WP_DEBUG', true);

and then after that add

    ini_set('log_errors',TRUE);
    ini_set('error_reporting', E_ALL);
    ini_set('error_log', dirname(__FILE__) . '/error_log.txt');

you will have an `error.log.txt` file in your root folder where all your logs are located. you can even log them with

    error_log( print_r( 'what I want to check goes here', true) );

inside your code. This will make your life a lot easier.

## WP_DEBUG
[`WP_DEBUG`](https://codex.wordpress.org/WP_DEBUG) is a PHP constant (a permanent global variable) that can be used to trigger the "debug" mode throughout WordPress. It is assumed to be false by default and is usually set to true in the [`wp-config.php`](https://codex.wordpress.org/Editing_wp-config.php) file on development copies of WordPress.

    define( 'WP_DEBUG', true );
    define( 'WP_DEBUG', false );

## WP_DEBUG_DISPLAY
[`WP_DEBUG_DISPLAY`](https://codex.wordpress.org/WP_DEBUG) is another companion to WP_DEBUG that controls whether debug messages are shown inside the HTML of pages or not. The default is 'true' which shows errors and warnings as they are generated. Setting this to false will hide all errors. This should be used in conjunction with WP_DEBUG_LOG so that errors can be reviewed later.
Note: for WP_DEBUG_DISPLAY to do anything, WP_DEBUG must be enabled (true).


    //enable
    define( 'WP_DEBUG_DISPLAY', true );
    
    //disable
    define( 'WP_DEBUG_DISPLAY', false );



## SCRIPT_DEBUG
[`SCRIPT_DEBUG`](https://codex.wordpress.org/Debugging_in_WordPress) is a related constant that will force WordPress to use the "dev" versions of core CSS and JavaScript files rather than the minified versions that are normally loaded. This is useful when you are testing modifications to any built-in .js or .css files. Default is false.
   
    //enable
    define( 'SCRIPT_DEBUG', true );

    //disable
    define( 'SCRIPT_DEBUG', false );


