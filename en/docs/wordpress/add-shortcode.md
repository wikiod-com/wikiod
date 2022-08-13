---
title: "Add Shortcode"
slug: "add-shortcode"
draft: false
images: []
weight: 9989
type: docs
toc: true
---

## Syntax
- `add_shortcode( $tag , $func );`

## Parameters
| Parameter | Details |
| ------ | ------ |
| $tag   | *(string) (required)* Shortcode tag to be searched in post content   |
| $func   | *(callable) (required)* Hook to run when shortcode is found   |

 - The shortcode callback will be passed three arguments: the shortcode attributes, the shortcode content (if any), and the name of the shortcode.
 - There can only be one hook for each shortcode. Which means that if another plugin has a similar shortcode, it will override yours or yours will override theirs depending on which order the plugins are included and/or ran.
 - Shortcode attribute names are always converted to lowercase before they are passed into the handler function. Values are untouched.
 - Note that the function called by the shortcode should never produce output of any kind. Shortcode functions should return the text that is to be used to replace the shortcode. Producing the output directly will lead to unexpected results. This is similar to the way filter functions should behave, in that they should not produce expected side effects from the call, since you cannot control when and where they are called from.


## Simple shortcode for recent post

`add_shortcode` is wp keyword.

    // recent-posts is going to be our shortcode.
    add_shortcode('recent-posts', 'recent_posts_function');
    
    // This function is taking action when recent post shortcode is called.
    function recent_posts_function() {
       query_posts(array('orderby' => 'date', 'order' => 'DESC' , 'showposts' => 1));
       if (have_posts()) :
          while (have_posts()) : the_post();
             $return_string = '<a href="'.get_permalink().'">'.get_the_title().'</a>';
          endwhile;
       endif;
       wp_reset_query();
       return $return_string;
    }

This snippet can be placed in your theme `functions.php`.

`[recent-posts]` This is out shortcode for recent post. We can apply this shortcode in backend (such as pages, post, widgets ).

We can also used same shortcode inside our code. with the help of `do_shortcode`.<br>
**Eg.** **`echo do_shortcode( '[recent-posts]' );`**


## Advanced shortcode for recent posts
This functions takes parameter for how many recent posts you want to display.

Ex : you want to display only five recent posts. Just passed the arguments with posts="5" (you can pass any number of recent posts that you want to display).

Function fetch only five recent posts from database.

    // recent-posts is going to be our shortcode.
    add_shortcode('recent-posts', 'recent_posts_function');

    // Functions takes parameter such as posts="5".
    function recent_posts_function($atts){
       extract(shortcode_atts(array(
          'posts' => 1,
       ), $atts));
    
       $return_string = '<ul>';
       query_posts(array('orderby' => 'date', 'order' => 'DESC' , 'showposts' => $posts));
       if (have_posts()) :
          while (have_posts()) : the_post();
             $return_string .= '<li><a href="'.get_permalink().'">'.get_the_title().'</a></li>';
          endwhile;
       endif;
       $return_string .= '</ul>';
    
       wp_reset_query();
       return $return_string;
    }

**Eg.** **`echo do_shortcode( '[recent-posts posts="5"]' );`**

