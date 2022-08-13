---
title: "Shortcode with attribute"
slug: "shortcode-with-attribute"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

## Syntax
- add_shortcode('your_short_code', 'your_function_name');

## Parameters
| Parameters | Discription and usage|
| ------ | ------ |
| $tag | (string) (required) Shortcode tag to be searched in post content Default: None|
| $func | (callable) (required) Hook to run when shortcode is found Default: None|


IMPORTANT – Don’t use camelCase or UPPER-CASE for your attributes

You can generate a shortcode with attribute [Here][1]


  [1]: https://generatewp.com/shortcodes/

## Examples of Shortcodes
WordPress shortcodes were introduced in 2.5

Here is come example of shortcode

    [button]
to use shortcode direct into theme you have to use `do_shortcode()`

     <?php echo do_shortcode('[button]'); ?>

To customize the button, we could simply add something like:

    [button type="twitter"]
Or to make it even better, we could use an enclosing shortcode:

    [button type="twitter"]Follow me on Twitter![/button]


## Creating a self-closing shortcode
The simplest shortcode is the self-closing one. We’re going to create a simple link to our Twitter account, and then add it in a blog post.
All the code goes in `functions.php`, which is located in `/wp-content/themes/your-theme/`. If you don’t have one, just create it and put the code in it.

    <?php 
    function button_shortcode() {
    return '<a href="http://twitter.com/rupomkhondaker" class="twitter-button">Follow me on Twitter!</a>"';
    }
    add_shortcode('button', 'button_shortcode'); 
    ?>

Usage: `[button]`



## Creating a self-closing shortcode with parameters
Creating a self-closing shortcode with parameters 

    <?php
    function button_shortcode( $type ) {

        extract( shortcode_atts( 
            array( 
                'type' => 'value'
             ), $type ) ); 

        // check what type user entered
        switch ($type) {

            case 'twitter':
                return '<a href="http://twitter.com/rupomkhondaker" class="twitter-button">Follw me on Twitter!</a>';
                break;

            case 'rss':
                return '<a href="http://example.com/rss" class="rss-button">Subscribe to the feed!</a>'
                break;
        }
    }
    add_shortcode( 'button', 'button_shortcode' );
    ?>

Now you can choose what button to display by defining type in your shortcode.

    [button type="twitter"]
    [button type="rss"]



## Creating an enclosing shortcode
enclosing shortcode

The enclosing shortcode allows you to embed content within your shortcode, just like BBCode if you’ve ever used that.

    <?php
    function button_shortcode( $attr, $content = null ) {
    return '<a href="http://twitter.com/filipstefansson" class="twitter-button">' . $content . '</a>';
    }
    add_shortcode('button', 'button_shortcode');
    ?>
To use this shortcode, you embedd the text you want to use like this:

    [button]Follow me on Twitter![/button]

To make this button even better, we could add parameters just like we did in the previous example.

    <?php
    function button_shortcode( $atts, $content = null ) {
    extract( shortcode_atts( array(
    'account' => 'account',
     'style' => 'style'
     ), $atts ) );
    return '<a href="http://twitter.com/' . esc_attr($account) . '" class="twitter-button ' . esc_attr($style) . '">' . $content . '</a>';
    }
    add_shortcode('button', 'button_shortcode');
    ?>

Usage: 

    [button account="rupomkhondaker" style="simple"]Follow me on Twitter![/button]

## Shortcodes in Widgets
By default, WordPress does not support shortcodes within Sidebar Widgets. It only expands the shortcodes within the content of a Post, Page, or custom post type. To add shortcode support to sidebar widgets, you can install a plugin, or use the below code:

    add_filter( 'widget_text', 'shortcode_unautop' );
    add_filter( 'widget_text', 'do_shortcode' );

It is important that these lines be added in this order. The first line prevents WordPress from turning line breaks into paragraph tags, since this keeps shortcodes from working. The second line is the one that makes the shortcodes work.

