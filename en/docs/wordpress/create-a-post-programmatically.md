---
title: "Create a Post Programmatically"
slug: "create-a-post-programmatically"
draft: false
images: []
weight: 9943
type: docs
toc: true
---

## Syntax
 - wp_insert_post(array $args, bool $wp_error);

## Parameters
| Parameter | Description |
| ------ | -------------------------- |
| $args (Array Required) | A Key Value Array of the below elements.|
| $wp_error (Boolean Optional) | Return a WP_Error in case of failure.|

----------

Arguments
======================

The next table shows you a list of elements that you can use inside of the first parameter (Array).

| Parameter | Description
| ------ | ------ |
| ID | (Int) The post ID. If equal to something other than 0, the post with that ID will be updated. Default 0.|
| post_author | (Int) The ID of the user who added the post. Default is the current user ID.|
| post_date | (String) The date of the post. Default is the current time.|
| post_date_gmt | (String) The date of the post in the GMT timezone. Default is the value of $post_date.|
| post_content | (Mixed) The post content. Default empty.|
| post_content_filtered | (String) The filtered post content. Default empty.|
| post_title | (String) The post title. Default empty.|
| post_category | (Array) Array of post category values.|
| post_excerpt (String) The post excerpt. Default empty.|
| post_status | (String) The post status. Default draft.|
| post_type | (String) The post type. Default post.|
| comment_status | (String) Whether the post can accept comments. Accepts open or closed. Default is the value of default_comment_status option.|
| ping_status | (String) Whether the post can accept pings. Accepts open or closed. Default is the value of default_ping_status option.|
| post_password | (String) The password to access the post. Default empty.|
| post_name | (String) The post name or slug. Default is the sanitized post title when creating a new post.|
| to_ping | (String) Space or carriage return-separated list of URLs to ping. Default empty.|
| pinged | (String) Space or carriage return-separated list of URLs that have been pinged. Default empty.|
| post_modified | (String) The date when the post was last modified. Default is the current time.|
| post_modified_gmt | (String) The date when the post was last modified in the GMT timezone. Default is the current time.|
| post_parent | (Int) Set this for the post it belongs to, if any. Default 0.|
| menu_order | (Int) The order the post should be displayed in. Default 0.|
| post_mime_type | (String) The mime type of the post. Default empty.|
| guid | (String) Global Unique ID for referencing the post. Default empty.|
| tax_input | (Array) Array of taxonomy terms keyed by their taxonomy name. Default empty.|
| meta_input | (Array) Array of post meta values keyed by their post meta key. Default empty.|

Avoid Duplicated Posts
======================
When you execute this function, you could probably get a duplicated post, at least that happened to me. (You can check it into the Post WordPress Section)

I found a [solution][1]:

    if( !get_page_by_title( $title, 'OBJECT', 'post' ) ){
        $my_post = array('post_title' => $title,
            'post_content' => 'Content',
            'tags_input' => $tags,
            'post_category' => array(2),
            'post_status' => 'publish'
        );
    
        $result = wp_insert_post( $my_post );
    }

Explanation
-----------

Before you save a new post, validate if the new post already exists using the post title as a parameter, if there's not a post title, you can save your new post.

Check get_page_by_title's documentation [here][2].

  [1]: http://wordpress.stackexchange.com/a/50858/99204
  [2]: https://codex.wordpress.org/Function_Reference/get_page_by_title

## Create a Basic Page
    $basic_page_args = array(
        'post_title' => 'My Basic Page',
        'post_content' => 'This is a basic content',
        'post_type' => 'page',
        'post_status' => 'publish',
        'post_author' => 1
    );

    wp_insert_post( $basic_page_args );

## Create a Basic Post
    $basic_post_args = array(
        'post_title' => 'My Basic Post',
        'post_content' => 'This is a basic content',
        'post_status' => 'publish',
        'post_author' => 1,
        'post_category' => array(8, 59)
    );

    wp_insert_post( $basic_post_args );

## Introduction
Sometimes we have another editor somewhere else instead of TinyMCE (Wordpress Default Editor). That happen when we are creating our own Theme, Plugin or something specific; and we need to write and manipulate a type of post and save it into our WP Database.

So, if you are on that situation, you can use a Wordpress Function called:

    wp_insert_post( array $args, bool $wp_error );

