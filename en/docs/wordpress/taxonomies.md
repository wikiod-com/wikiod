---
title: "Taxonomies"
slug: "taxonomies"
draft: false
images: []
weight: 9985
type: docs
toc: true
---

## Syntax
- register_taxonomy( $taxonomy, $object_type, $args );

## Parameters
| Parameter | Details |
| ------ | ------ |
| $taxonomy   | *(string) (required)* The name of the taxonomy. Name should only contain lowercase letters and the underscore character, and not be more than 32 characters long (database structure restriction). |
| $object_type | *(array/string) (required)* Name of the object type for the taxonomy object. Object-types can be built-in Post Type or any Custom Post Type that may be registered. |
| $args | *(array/string) (optional)* An array of Arguments. |

## Example of registering a taxonomy for genres
    <?php
    // hook into the init action and call create_book_taxonomies when it fires
    add_action( 'init', 'create_book_taxonomies', 0 );
    
    // create taxonomy genres for the post type "book"
    function create_book_taxonomies() {
        // Add new taxonomy, make it hierarchical (like categories)
        $labels = array(
            'name'              => _x( 'Genres', 'taxonomy general name' ),
            'singular_name'     => _x( 'Genre', 'taxonomy singular name' ),
            'search_items'      => __( 'Search Genres' ),
            'all_items'         => __( 'All Genres' ),
            'parent_item'       => __( 'Parent Genre' ),
            'parent_item_colon' => __( 'Parent Genre:' ),
            'edit_item'         => __( 'Edit Genre' ),
            'update_item'       => __( 'Update Genre' ),
            'add_new_item'      => __( 'Add New Genre' ),
            'new_item_name'     => __( 'New Genre Name' ),
            'menu_name'         => __( 'Genre' ),
        );
    
        $args = array(
            'hierarchical'      => true,
            'labels'            => $labels,
            'show_ui'           => true,
            'show_admin_column' => true,
            'query_var'         => true,
            'rewrite'           => array( 'slug' => 'genre' ),
        );
    
        register_taxonomy( 'genre', array( 'book' ), $args );
        
    }
    ?>

You can define custom taxonomies in a themes's `functions.php` template file:

## Add category in page
You can also add same custom created taxonomy into post type page using below code.

    function add_taxonomies_to_pages() {
         register_taxonomy_for_object_type( 'genre', 'page' );
     }
    add_action( 'init', 'add_taxonomies_to_pages' );
Add above code into your theme's functions.php file. Same way you can add custom or default `post_tag` into post type page.

To get pages using custom taxonomy query need to add below code in same file.

    if ( ! is_admin() ) {
         add_action( 'pre_get_posts', 'category_and_tag_archives' );
     }

    function category_and_tag_archives( $wp_query ) {
        $my_post_array = array('page');
        if ( $wp_query->get( 'category_name' ) || $wp_query->get( 'cat' ) )
        $wp_query->set( 'post_type', $my_post_array );
    }

## Add Categories and Tags to Pages and insert them as class into <body>
    // add tags and categories to pages

    function add_taxonomies_to_pages() {
     register_taxonomy_for_object_type( 'post_tag', 'page' );
     register_taxonomy_for_object_type( 'category', 'page' );
     }
    add_action( 'init', 'add_taxonomies_to_pages' );
     if ( ! is_admin() ) {
     add_action( 'pre_get_posts', 'category_and_tag_archives' );
     
     }
    function category_and_tag_archives( $wp_query ) {
    $my_post_array = array('post','page');
     
     if ( $wp_query->get( 'category_name' ) || $wp_query->get( 'cat' ) )
     $wp_query->set( 'post_type', $my_post_array );
     
     if ( $wp_query->get( 'tag' ) )
     $wp_query->set( 'post_type', $my_post_array );
    }
    
    // add tags and categorys as class to <body>

    function add_categories_and_tags( $classes = '' ) {
        if( is_page() ) {
            $categories = get_the_category();
            foreach( $categories as $category ) {
                $classes[] = 'category-'.$category->slug;
            }
            $tags = get_the_tags();
            foreach( $tags as $tag ) {
                $classes[] = 'tag-'.$tag->slug;
            }
        }
    return $classes;
    }
    add_filter( 'body_class', 'add_categories_and_tags' );

## Add Categories and Tags to Pages and insert as class into <body>
You can add this code to your custom functions.php file:

    // add tags and categories to pages
    
    function add_taxonomies_to_pages() {
        register_taxonomy_for_object_type( 'post_tag', 'page' );
        register_taxonomy_for_object_type( 'category', 'page' );
    }
    add_action( 'init', 'add_taxonomies_to_pages' );
    
    if ( ! is_admin() ) {
        add_action( 'pre_get_posts', 'category_and_tag_archives' );
    }
     
    function category_and_tag_archives( $wp_query ) {
    $my_post_array = array('post','page');
    
        if ( $wp_query->get( 'category_name' ) || $wp_query->get( 'cat' ) )
        $wp_query->set( 'post_type', $my_post_array );
        
        if ( $wp_query->get( 'tag' ) )
        $wp_query->set( 'post_type', $my_post_array );
        
    }
    
    // add tags and categorys as class to <body>
    
    function add_categories_and_tags( $classes = '' ) {
        if( is_page() ) {
            $categories = get_the_category();
            foreach( $categories as $category ) {
                $classes[] = 'category-'.$category->slug;
            }
            $tags = get_the_tags();
            foreach( $tags as $tag ) {
                $classes[] = 'tag-'.$tag->slug;
            }
        }
    return $classes;
    }
    add_filter( 'body_class', 'add_categories_and_tags' );

Works perfect, tested in **WordPress 4.8**

