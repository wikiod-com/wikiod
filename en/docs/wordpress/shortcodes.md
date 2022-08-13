---
title: "Shortcodes"
slug: "shortcodes"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

## Shortcode introduction
Shortcodes are useful when you want to be able add more complex elements inline into the normal content editor.

A shortcode in it's simplest for would look like this:

    function my_shortcode( ){
        return "This is a shortcode";
    }
    add_shortcode( 'my_shortcode', 'my_shortcode' );

It would output the text `"This is a shortcode"` and you would use it by writing [my_shortcode] in the content editor. 

## Button shortcode
Here is an example of a simple button short code:

    <?php
    function my_button_shortcode( $atts ) {

        // Parse the input attributes and assgn default values for the 
        // attributes that are not specified on the shortcode
        $a = shortcode_atts( array(
            'id' => '',
            'url' => '#',
            'class' => '',
            'text' => ''
        ), $atts );

        // Open the anchor tag and add role=button for better accessibility
        $btn_html = '<a role="button"';

        // Add the href(link) attribute
        $btn_html .= ' href="' . $a['url'] . '"';

        // Add id attribute to output if specified
        if ( !empty( $a['id'] ) ) {
            $btn_html .= ' id="' . $a['id'] . '"';
        }

        $btn_classes = 'button';


        // Add class attribute to output
        $btn_html .= ' class="button ' . ( !empty(a['class']) ? $a['class'] : '' ) . '"';

        // Close opening anchor tag
        $btn_html .= '>'.
            // Add button text
            $a['text'].
            // Add closing anchor tag
            '</a>'."\n";

        return $btn_html;
    }
    add_shortcode( 'button', 'my_button_shortcode' );

This shortcode can be used by typing `[button url="/my-other-page" id="my-other-page-button" class="dark" text="Click me!"]` into the editor and will output the following HTML:

    <a role="button" href="/my-other-page" id="my-other-page-button" 
    class="button dark">Click me!</a>

