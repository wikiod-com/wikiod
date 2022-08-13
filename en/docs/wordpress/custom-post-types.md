---
title: "Custom Post Types"
slug: "custom-post-types"
draft: false
images: []
weight: 9854
type: docs
toc: true
---

## Syntax
- register_post_type( $post_type, $args );

## Parameters
 | Parameter |  Details  |
 |-----------|-----------|
 | $post_type | (string) (Required)
 | $args      | (array/string) (Optional) 


## Add Custom Post Types to Main Query
Registering a custom post type does not mean it gets added to the main query automatically. You need to use `pre_get_posts` filter to add custom post types to main query.

    // Show posts of 'post' and 'book' custom post types on home page
    add_action( 'pre_get_posts', 'add_my_post_types_to_query' );
    
    function add_my_post_types_to_query( $query ) {
      if ( is_home() && $query->is_main_query() )
        $query->set( 'post_type', array( 'post', 'book' ) );
      return $query;
    }

## Registering a Custom Post Type
Say you have a library website, and you want to have a custom post type named *Books*. It can be registered as

    
    function create_bookposttype() {
        $args = array(
            'public' => true,
            'labels' => array(
                'name' => __( 'Books' ),
                'singular_name' => __( 'Book' )
            ),
        );
        register_post_type( 'custompost_books', $args );
    }
    
    add_action( 'init', 'create_bookposttype' );

   

and, as simple as that is, you now have a custom post type registered.

[![Custom Post 'Book' Screenshot][1]][1]

This snippet can be placed in your theme `functions.php` file, or within a plugin structure.




  [1]: http://i.stack.imgur.com/35Ec7.jpg

## Adding Custom Post Types to Main RSS Feed
Registering a custom post type does not mean it gets added to the main RSS feed automatically.You need to use `request` filter to add custom post types to main RSS feed.

    // Add 'books' custom post types on main RSS feed
    function add_book_post_types_to_rss($qv) {
        if (isset($qv['feed']) && !isset($qv['post_type']))
            $qv['post_type'] = array('post', 'books', );
        return $qv;
    }
    add_filter('request', 'add_book_post_types_to_rss');

## Register Custom Post Type
    if ( ! function_exists('products_post_type') ) {
    
    function products_post_type() {
    
        $labels = array(
            'name'                  => _x( 'Products', 'Post Type General Name', 'text_domain' ),
            'singular_name'         => _x( 'Product', 'Post Type Singular Name', 'text_domain' ),
            'menu_name'             => __( 'Products', 'text_domain' ),
            'name_admin_bar'        => __( 'Product', 'text_domain' ),
            'archives'              => __( 'Item Archives', 'text_domain' ),
            'attributes'            => __( 'Item Attributes', 'text_domain' ),
            'parent_item_colon'     => __( 'Parent Product:', 'text_domain' ),
            'all_items'             => __( 'All Products', 'text_domain' ),
            'add_new_item'          => __( 'Add New Product', 'text_domain' ),
            'add_new'               => __( 'New Product', 'text_domain' ),
            'new_item'              => __( 'New Item', 'text_domain' ),
            'edit_item'             => __( 'Edit Product', 'text_domain' ),
            'update_item'           => __( 'Update Product', 'text_domain' ),
            'view_item'             => __( 'View Product', 'text_domain' ),
            'view_items'            => __( 'View Items', 'text_domain' ),
            'search_items'          => __( 'Search products', 'text_domain' ),
            'not_found'             => __( 'No products found', 'text_domain' ),
            'not_found_in_trash'    => __( 'No products found in Trash', 'text_domain' ),
            'featured_image'        => __( 'Featured Image', 'text_domain' ),
            'set_featured_image'    => __( 'Set featured image', 'text_domain' ),
            'remove_featured_image' => __( 'Remove featured image', 'text_domain' ),
            'use_featured_image'    => __( 'Use as featured image', 'text_domain' ),
            'insert_into_item'      => __( 'Insert into item', 'text_domain' ),
            'uploaded_to_this_item' => __( 'Uploaded to this item', 'text_domain' ),
            'items_list'            => __( 'Items list', 'text_domain' ),
            'items_list_navigation' => __( 'Items list navigation', 'text_domain' ),
            'filter_items_list'     => __( 'Filter items list', 'text_domain' ),
        );
        $args = array(
            'label'                 => __( 'Product', 'text_domain' ),
            'description'           => __( 'Product information pages.', 'text_domain' ),
            'labels'                => $labels,
            'supports'              => array( 'title', 'editor', 'excerpt', 'author', 'thumbnail', 'comments', 'custom-fields', ),
            'taxonomies'            => array( 'category', 'post_tag' ),
            'hierarchical'          => false,
            'public'                => true,
            'show_ui'               => true,
            'show_in_menu'          => true,
            'menu_position'         => 5,
            'menu_icon'             => 'dashicons-products',
            'show_in_admin_bar'     => true,
            'show_in_nav_menus'     => true,
            'can_export'            => true,
            'has_archive'           => true,        
            'exclude_from_search'   => false,
            'publicly_queryable'    => true,
            'capability_type'       => 'page',
            'show_in_rest'          => true,
        );
        register_post_type( 'product', $args );
    
    }
    add_action( 'init', 'products_post_type', 0 );
    
    }

## Custom Post Type  using Twenty Fifteen WordPress Theme
You can use any name for the function.

    function custom_postype(){
        register_post_type('cus_post',array(
    
            'labels'=>array(
                'name'=>'khaiyam'// Use any name you want to show in menu for your users
                ),
            'public'=>true,// **Must required 
            'supports'=>array('title','editor','thumbnail')// Features you want to provide on your posts
            )); 
    }
    add_action('after_setup_theme','custom_postytpe'); 

or

    add_action('init','custom_postytpe');

You can use any of the hooks you want but of course they have different meaning and uses.


[![Simple-custom Post Type ][1]][1]


  [1]: https://i.stack.imgur.com/APbiA.png

## Custom post type in default search
You can add custom post type posts on default wordpress search, Add below code in theme functions.php

    function my_search_filter($query) {
      if ( !is_admin() && $query->is_main_query() ) {
        if ($query->is_search) {
          $query->set('post_type', array( 'news','post','article' ) );
        }
      }
    }
    add_action('pre_get_posts','my_search_filter');

