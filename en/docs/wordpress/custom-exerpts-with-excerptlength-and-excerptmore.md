---
title: "Custom exerpts with excerpt_length and excerpt_more"
slug: "custom-exerpts-with-excerpt_length-and-excerpt_more"
draft: false
images: []
weight: 9994
type: docs
toc: true
---

## Limit excerpt length to 50 words
Put the following code in **functions.php**:

    function themify_custom_excerpt_length( $length ) {
       return 50;
    }
    add_filter( 'excerpt_length', 'themify_custom_excerpt_length', 999 );

Use 999 as the priority to ensure that the function runs after the default WordPress filter, otherwise it would override what is set here.

## Adding a Read More link at the end of the excerpt
To do this, put the following code in **functions.php**:

    function custom_excerpt_more($more) {
       return '<a href="'. get_permalink($post->ID) . '">Read More</a>';
    }
    add_filter('excerpt_more', 'custom_excerpt_more');

The results should look like this:

[![Real world example from swiftsrbija.rs][1]][1]


  [1]: http://i.stack.imgur.com/nqUom.png

## Adding a few dots at the end of the excerpt
In our **functions.php**

    function new_excerpt_more( $more ) {
        return '.....';
    }
    add_filter('excerpt_more', 'new_excerpt_more');

We should get this:

[![Real world example from swiftsrbija.rs website][1]][1]


  [1]: http://i.stack.imgur.com/7XU0R.png

