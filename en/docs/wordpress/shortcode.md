---
title: "Shortcode"
slug: "shortcode"
draft: false
images: []
weight: 9889
type: docs
toc: true
---

## Adding New Shortcodes
    function footag_func( $atts ) {
        return "foo = {$atts['foo']}";
    }
    add_shortcode( 'footag', 'footag_func' );

In plugins we can add shortcodes using the add_shortcode function. 

The shortcode can be used in any Wordpress page or post just by enclosing it in square brackets.

``` 
[footag]
```

## Registering shortcode
Shortcode is a small piece of code that can be added into the WordPress editor and will output something different once the page is published or previewed.

Frequently, shortcodes are added to the theme `functions.php` file, but that's [not a good practice][1] as shortcodes are expected to keep working after changing themes. Instead, [write a plugin][2] to add this functionality.

The structure for registering shortcode is:

    function new_shortcode($atts, $content = null){
        // if parameters are needed in the shortcode
        // parameters can be set to default to something
        extract( shortcode_atts( array(
            'param_one' => 'h1'
        ), $atts ) );
        $shortcode = '<'.$param_one'>'.$content.'</'.$param_one.'>';
        return $shortcode;
    }
    // this is what registers the shortcode with wordpress
    add_shortcode('demo-shortcode','new_shortcode');

Inside the WordPress editor, you can type:

    [demo-shortcode param_one="h2"]Demo[/demo-shortcode]
    // you don't need to insert param_one into the editor if it has a default value.
    // having it in the editor will override the default

Once the page is published, this will turn into 

    <h2>Demo</h2>


  [1]: https://make.wordpress.org/themes/handbook/review/required/explanations-and-examples/
  [2]: https://wordpress.stackexchange.com/questions/73031/where-to-put-my-code-plugin-or-functions-php

## Using Shortcodes Inside PHP Code (themes and plugins)
    <?php echo do_shortcode("[footag foo='Hi! I am a foo output']"); ?>
To print a shortcode using php use the `do_shortcode` function and echo the returned value.

## Using Shortcodes in Widgets
    add_filter( 'widget_text', 'shortcode_unautop' );
    add_filter( 'widget_text', 'do_shortcode' );enter code here

Add this to a plugin or the `functions.php` file to enable shortcodes in widgets. The code first stops WordPress turning line breaks into paragraph tags and then lets shortcodes to parse for widgets. The order of the two lines is important.

## Using Shortcodes in WordPress Backend
    [footag foo="value of 1" attribute-2="value of 2"]
In wordpress admin we use pre defined shortcodes by writing the shortcode name inside square brackets and optionally adding attributes to it separating by space.

