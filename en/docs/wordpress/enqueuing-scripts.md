---
title: "Enqueuing scripts"
slug: "enqueuing-scripts"
draft: false
images: []
weight: 9958
type: docs
toc: true
---

## Syntax
- wp_enqueue_script( $handle, $src, $deps, $ver, $in_footer)




## Parameters
| Parameter | Details |
| --------- | ------- |
| *$handle* | *(string)* (Required) Name of the script. Should be unique. |
| *$src* | *(string)* (Optional) Full URL of the script, or path of the script relative to the WordPress root directory. *Default value: false* |
| *$deps* | *(array)* (Optional) An array of registered script handles this script depends on. *Default value: array()* |
| *$ver* | *(string \| bool \| null)* (Optional) String specifying script version number, if it has one, which is added to the URL as a query string for cache busting purposes. If version is set to false, a version number is automatically added equal to current installed WordPress version. If set to null, no version is added. *Default value: false* |
| *$in_footer* | *(bool)* (Optional) Whether to enqueue the script before `</body>` instead of in the `<head>`. *Default value: false* |

## Enqueuing scripts in functions.php
If you want to add `custom.js` script that is located in the `js/` folder of your theme, you'll need to enqueue it. In `functions.php` add

    <?php
    
    add_action( 'after_setup_theme', 'yourtheme_theme_setup' );
    
    if ( ! function_exists( 'yourtheme_theme_setup' ) ) {
        function yourtheme_theme_setup() {
    
            add_action( 'wp_enqueue_scripts', 'yourtheme_scripts' );
            add_action( 'admin_enqueue_scripts', 'yourtheme_admin_scripts' );
    
        }
    }
    
    if ( ! function_exists( 'yourtheme_scripts' ) ) {
        function yourtheme_scripts() {
    
            wp_enqueue_script( 'yourtheme_custom', get_template_directory_uri().'/js/custom.js', array( 'jquery' ), '1.0.0', true );
    
        }
    }
    
    if ( ! function_exists( 'yourtheme_admin_scripts' ) ) {
        function yourtheme_admin_scripts() {
    
            wp_enqueue_script( 'yourtheme_custom', get_template_directory_uri().'/js/custom.js', array( 'jquery-ui-autocomplete', 'jquery' ), '1.0.0', true );
    
        }
    }

## Enqueue scripts for IE only
    add_action( 'wp_enqueue_scripts', 'enqueue_my_styles_and_scripts' );
    
    /**
     * Enqueue scripts (or styles) conditionally.
     *
     * Load scripts (or stylesheets) specifically for IE. IE10 and above does
     * not support conditional comments in standards mode.
     *
     * @link https://gist.github.com/wpscholar/4947518
     * @link https://msdn.microsoft.com/en-us/library/ms537512(v=vs.85).aspx
     */
    function enqueue_my_styles_and_scripts() {
    
         // Internet Explorer HTML5 support
        wp_enqueue_script( 'html5shiv',get_template_directory_uri().'/js/html5shiv.js', array(), '3.7.3', false);
        wp_script_add_data( 'html5shiv', 'conditional', 'lt IE 9' );
    
        // Internet Explorer 8 media query support
        wp_enqueue_script( 'respond', get_template_directory_uri().'/js/respond.js', array(), '1.4.2', false);
        wp_script_add_data( 'respond', 'conditional', 'lt IE 9' );
    
    }

## Enqueuing Scripts conditionally for specific pages
You can use conditional operators in WordPress to enqueue scripts on specific pages of your Website.

    function load_script_for_single_post(){
        if(is_single()){
            wp_enqueue_script(
                    'some',
                    get_template_directory_uri().'/js/some.js',
                    array('jquery),
                    '1.0.0', 
                    false
            );
    
        }
    } 
    add_action('wp_enqueue_scripts','load_script_for_single_post');
In the above example, if the current webpage is single post, script will be enqueued. Otherwise *wp_enqueue_script* function will not be executed.


