---
title: "Querying posts"
slug: "querying-posts"
draft: false
images: []
weight: 9981
type: docs
toc: true
---

## Syntax
 - $the_query = new WP_Query( $args );
 - $posts_array = get_posts( $args );

## Parameters

|Parameter | Description |
| ------ | ------ |
| *$args* | *(array)* An array of needed arguments for a query - can be custom tailored to your needs, e.g. querying posts from only one category, from custom post type or even querying certain taxonomy   |

Query arguments are numerous. [WP_Query() codex](https://codex.wordpress.org/Class_Reference/WP_Query#Parameters) page has a list of parameters. Some of them are 

 - [Author Parameters](https://codex.wordpress.org/Class_Reference/WP_Query#Author_Parameters)
 - [Category Parameters](https://codex.wordpress.org/Class_Reference/WP_Query#Category_Parameters)
 - [Tag Parameters](https://codex.wordpress.org/Class_Reference/WP_Query#Tag_Parameters)
 - [Taxonomy Parameters](https://codex.wordpress.org/Class_Reference/WP_Query#Taxonomy_Parameters)
 - [Search Parameter](https://codex.wordpress.org/Class_Reference/WP_Query#Search_Parameter)
 - [Post & Page Parameters](https://codex.wordpress.org/Class_Reference/WP_Query#Post_.26_Page_Parameters)
 - [Password Parameters](https://codex.wordpress.org/Class_Reference/WP_Query#Password_Parameters)
 - [Type Parameters](https://codex.wordpress.org/Class_Reference/WP_Query#Type_Parameters)
 - [Status Parameters](https://codex.wordpress.org/Class_Reference/WP_Query#Status_Parameters)
 - [Pagination Parameters](https://codex.wordpress.org/Class_Reference/WP_Query#Pagination_Parameters)
 - [Order & Orderby Parameters](https://codex.wordpress.org/Class_Reference/WP_Query#Order_.26_Orderby_Parameters)
 - [Date Parameters](https://codex.wordpress.org/Class_Reference/WP_Query#Date_Parameters)
 - [Custom Field Parameters](https://codex.wordpress.org/Class_Reference/WP_Query#Custom_Field_Parameters)
 - [Permission Parameters](https://codex.wordpress.org/Class_Reference/WP_Query#Permission_Parameters)
 - [Mime Type Parameters](https://codex.wordpress.org/Class_Reference/WP_Query#Mime_Type_Parameters)
 - [Caching Parameters](https://codex.wordpress.org/Class_Reference/WP_Query#Caching_Parameters)
 - [Return Fields Parameter](https://codex.wordpress.org/Class_Reference/WP_Query#Return_Fields_Parameter)

One of the most important thing to have in mind is:

## **Never use query_posts()** ##

`query_posts()` overrides the main query, and can cause problems in the rest of your theme. Any time you need to modify the main query (or any query for that matter) is to use [pre_get_posts](https://codex.wordpress.org/Plugin_API/Action_Reference/pre_get_posts) filter. This will allow you to modify the query before it ran.

Also when you are querying posts, you should always reset it using [wp_reset_postdata()](https://codex.wordpress.org/Function_Reference/wp_reset_postdata). This will restore the global `$post` variable of the main query loop, and you won't have any issues later on (such as categories being excluded, because in your secondary loop you've excluded them and forgot to reset the query).

## Using WP_Query() object
Creating a separate instance of the `WP_Query` object is easy:

    $query_args = array(
                    'post_type' => 'post',
                    'post_per_page' => 10
                ); 
    
    $my_query = new WP_Query($query_args);
    
    if( $my_query->have_posts() ):
        while( $my_query->have_posts() ): $my_query->the_post();
           //My custom query loop
        endwhile;
    endif;
    
    wp_reset_postdata();

Notice that you need to build the query arguments array to your specification. For more details, look at [WP_Query codex page](https://codex.wordpress.org/Class_Reference/WP_Query).

## Using get_posts()
`get_posts()` is a wrapper for a separate instance of a `WP_Query` object. The returned value is an array of post object.

    global $post;

    $args = array(
        'numberposts' => 5,
        'offset'=> 1,
        'category' => 1
    );

    $myposts = get_posts( $args );

    foreach( $myposts as $post ) :
        setup_postdata($post); ?>
        <h2><a href="<?php the_permalink(); ?>"><?php the_title(); ?></a></h2>
    <?php endforeach;
    wp_reset_postdata(); ?>

For more info check out the [get_posts() codex page](https://codex.wordpress.org/Template_Tags/get_posts).

