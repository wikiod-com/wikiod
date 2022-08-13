---
title: "The Loop (main WordPress loop)"
slug: "the-loop-main-wordpress-loop"
draft: false
images: []
weight: 9886
type: docs
toc: true
---

## Alternative loop syntax
You can also use loop with curly brackets like this:

    if ( have_posts() ) {
        while ( have_posts() ) {

            the_post(); 
            var_dump( $post );
        
        }
    }

## Basic WordPress loop structure
Each time WordPress loads the page, it will run *main loop*.

The loop is the way to iterate over all elements related to the page you are currently on.

Main loop will work on a global `WP_Query` object. The query has a globalized method `have_posts()`, that allows us to loop through all results. Finally inside the loop you can call `the_post()` method (also as a global function), which sets global post object to the current post inside the loop, and sets the postdata to the current post. Thanks to this you can call functions like `the_title`, `the_content`, `the_author` (*template tags*) directly inside the loop.

For example if you are on posts lists, main loop will contain a query object with all posts.

If you are on single post (or page), it will contain a query with single post (page) you are currently on.

    if ( have_posts() ) : 
        while ( have_posts() ) :
            the_post();
            var_dump( $post );
        endwhile;
    endif;

## Handling no items in the loop
If you want to handle such scenario just add an `if/else` statement.

    if ( have_posts() ) : while ( have_posts() ) : 
    
        the_post(); 
        var_dump( $post );
    
    endwhile; else :
    
        __('This Query does not have any results');    
    
    endif;

