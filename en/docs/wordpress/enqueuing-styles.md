---
title: "Enqueuing Styles"
slug: "enqueuing-styles"
draft: false
images: []
weight: 9972
type: docs
toc: true
---

## Syntax
1. wp_enqueue_style($handle, $src, $dependency, $version, $media);

## Parameters
| Parameter  | Details |
| ---------- | ------- |
| `$handle`  | (String) (Required) Unique name for the stylesheet. |
| `$src`  | (String) (Optional) URL of stylesheet which will be used inside **link** tag's src attribute. |
| `$deps`  | (String) (Optional) An array of stylesheet handles this stylesheet depends on. |
| `$ver`  | (String) (Optional) String specifying stylesheet version of stylesheet. |
| `$media`  | (String) (Optional) The media for which this stylesheet is created. e.g  'all', 'print', 'screen' etc |

## Including internal css file with another css file as a dependency
    function themeSlug_enqueue_scripts() {
        wp_enqueue_style( 'themeSlug-reset', get_template_directory_uri() .'/css/reset.css', '1.0.0' );
        wp_enqueue_style( 'themeSlug-style', get_template_directory_uri() .'/style.css', 'themeSlug-reset', '1.0.0');
    }
    add_action('wp_enqueue_scripts', 'themeSlug_enqueue_scripts');

## Including internal css file
In this case `style.css` is located in root of the theme's folder

    function themeSlug_enqueue_scripts() {
        wp_enqueue_style( 'themeSlug-style', get_template_directory_uri() .'/style.css', '1.0.0');
    }
    add_action('wp_enqueue_scripts', 'themeSlug_enqueue_scripts');

## Including external css file
In this example we want to include font awesome icon font

    function themeSlug_enqueue_scripts() {
        wp_enqueue_style( 'font-awesome', '//cdnjs.cloudflare.com/ajax/libs/font-awesome/4.6.3/css/font-awesome.css');
    }
    add_action('wp_enqueue_scripts', 'themeSlug_enqueue_scripts');

## Enqueue stylesheets for IE only
    add_action( 'wp_enqueue_scripts', 'enqueue_my_styles_and_scripts' );
    
    /**
     * Enqueue styles (or scripts) conditionally.
     *
     * Load stylesheets (or scripts) specifically for IE. IE10 and above does
     * not support conditional comments in standards mode.
     *
     * @link https://gist.github.com/wpscholar/4947518
     * @link https://msdn.microsoft.com/en-us/library/ms537512(v=vs.85).aspx
     */
    function enqueue_my_styles_and_scripts() {
    
        // Internet Explorer specific stylesheet.
        wp_enqueue_style( 'themename-ie', get_stylesheet_directory_uri() . '/css/ie.css', array( 'twentyfifteen-style' ), '20141010' );
        wp_style_add_data( 'themename-ie', 'conditional', 'lte IE 9' );
    
        // Internet Explorer 7 specific stylesheet.
        wp_enqueue_style( 'themename-ie7', get_stylesheet_directory_uri() . '/css/ie7.css', array( 'twentyfifteen-style' ), '20141010' );
        wp_style_add_data( 'themename-ie7', 'conditional', 'lt IE 8' );
    
    }


## Including internal css file for your Plugin class
<pre>
<code>
class My_Plugin() {
  function __construct() {
    add_action( 'wp_enqueue_scripts', array( $this, 'init_fe_assets' ) );
  }

  public function init_fe_assests() {
    wp_enqueue_style( 'my-plugin', plugin_dir_url( __FILE__ ) . 'assets/css/frontend/plugin.css', array(), '0.0.1', true );
  }
}

new My_Plugin();
</code>
</pre>

## Add Alternative Stylesheets
        <?php wp_enqueue_style('theme-five', get_template_directory_uri() . '/path/to/additional/css'); 
    wp_style_add_data('theme-five', 'alt', true); 
    wp_style_add_data('theme-five', 'title', __('theme-five.css', 'your-theme-name')); ?>

[wp_style_add_data][1]


  [1]: https://developer.wordpress.org/reference/functions/wp_style_add_data/

