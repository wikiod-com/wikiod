---
title: "Alternating main loop (pre_get_posts filter)"
slug: "alternating-main-loop-pre_get_posts-filter"
draft: false
images: []
weight: 9976
type: docs
toc: true
---

## Syntax
 - add_action( 'pre_get_posts', 'callback_function_name' );
 - function callback_function_name( $query ) {}
 - // for PHP 5.3.0 or above
 - add_action( 'pre_get_posts', function( $query ){} );

## Parameters
| Parameter | Details |
| --------- | ------- |
| *$query*   | *(WP_Query)* Loop object |


If you are using PHP 5.3.0 or above, you can use closures ([anonymous functions][1])

    add_action( 'pre_get_posts', function( $query ) {
        if( !$query->is_main_query() || is_admin() ) return;
    
        // this code will run only if
        // - this query is main query
        // - and this is not admin screen
    });



  [1]: http://uk.php.net/functions.anonymous

## Change posts_per_page for main loop
All we need to do is to use `set()` method of `$query` object.

It takes two arguments, first what we want to set and second what value to set.
    
    add_action( 'pre_get_posts', 'change_posts_per_page' );

    function change_posts_per_page( $query ) {
        if( !$query->is_main_query() || is_admin() ) return;
    
        $query->set( 'posts_per_page', 5 );
        return;
    }

## Even more specific loop targeting
Let's say we want to change *[main loop][1]*, only for specific taxonomy, or post type.

Targeting only main loop on `book` post type archive page.

    add_action( 'pre_get_posts', 'my_callback_function' );
    
    function my_callback_function( $query ) {
        if( !$query->is_main_query() || is_admin() ) return;
        if( !is_post_type_archive( 'book' ) ) return;
    
        // this code will run only if
        // - this query is main query
        // - and this is not admin screen
        // - and we are on 'book' post type archive page
    }

You can also check for category, tag or custom taxonomy archive page using `is_category()`, `is_tag()` and `is_tax()`.

You can use any *conditional tag* available in WordPress.


  [1]: https://www.wikiod.com/wordpress/the-loop-main-wordpress-loop

## Show posts from only one category
    add_action( 'pre_get_posts', 'single_category' );
    
    function single_category( $query ) {
        if( !$query->is_main_query() || is_admin() ) return;
    
        $query->set( 'cat', '1' );
        return;
    }

## Pre get posts filter basic usage
Sometimes you would like to change main WordPress query.

Filter `pre_get_posts` is the way to go.

For example using `pre_get_posts` you can tell *[main loop][1]* to show only 5 posts. Or to show posts only from one category, or excluding any category etc.

    add_action( 'pre_get_posts', 'my_callback_function' );
    
    function my_callback_function( $query ) {
        // here goes logic of your filter
    }

As you can see, we are passing *[main loop][1]* query object into our callback function argument.

Important note here: **we are passing argument as a reference**. It means that we do not need to return query or set any globals to get it working. As `$query` is a reference to the main query object, all changes we make on our object are immediately reflected in the main loop object.

[1]: https://www.wikiod.com/wordpress/the-loop-main-wordpress-loop

## Exclude category from posts list edit share
    add_action( 'pre_get_posts', 'single_category_exclude' );
    
    function single_category_exclude( $query ) {
        if( !$query->is_main_query() || is_admin() ) return;
    
        $query->set( 'cat', '-1' );
        return;
    }

## Targeting only main WordPress loop
WordPress is applying `pre_get_posts` filter to literally any loop it generates. It means that all changes we are making in our callback function are applied to all exiting loops.

Obviously it is not what we want in most scenarios.

In most cases we would like to target only *[main loop][1]*, and only for non-admin screens.

We can use `is_main_query()` method and `is_admin()` global function to check if we are in the right place.

    add_action( 'pre_get_posts', 'my_callback_function' );
    
    function my_callback_function( $query ) {
        if( !$query->is_main_query() || is_admin() ) return;

        // this code will run only if
        // - this query is main query
        // - and this is not admin screen
    }

[1]: https://www.wikiod.com/wordpress/the-loop-main-wordpress-loop

