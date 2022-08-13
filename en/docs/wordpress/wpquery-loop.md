---
title: "WP_Query() Loop"
slug: "wp_query-loop"
draft: false
images: []
weight: 9966
type: docs
toc: true
---

WP_Query to query for posts, pages and custom post types. You will get list for specific posts and pages or custom post types.


WP_Query allows you to pull posts from the database according to your criteria.

## Retrieve latest 10 posts
    $args = array(
        'post_type'=>'post',
        'posts_per_page' =>'10'
    );
    $latest_posts_query = new WP_Query( $args );
    if($latest_posts_query->have_posts()) :
    while ( $latest_posts_query-> have_posts()) : $latest_posts_query->the_post();
    //Get post details here
    endwhile;
    endif;



