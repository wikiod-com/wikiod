---
title: "Getting started with WordPress"
slug: "getting-started-with-wordpress"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Introduction to WordPress
[WordPress](https://wordpress.org) [WP] is an open source Content Management System for building apps, websites, and blogs. WP is written in PHP and uses MySQL as the data store for the user content and configuration. It has a rich ecosystem of [plugins][1] and [themes][2] and enjoys a vibrant open source community, good documentation, and low barriers to entry. Usability and developer documentation can be found in the [WP Codex][3].

A part of WordPress that makes it different from most other CMS products is its [Event Driven Programming][4]. This is a different way of programming and logic representation then the MVC (Model View Controller) architecture which is used by most of the CMS systems. WordPress uses the concepts of Actions and Filters. They form a queue of events that allow plugins and themes to insert, modify or even remove parts of the final web application page and/or parts. A similar concept is JIT or Just-In-Time compiling. 

While historically WordPress has been known as a blogging platform, and it may never lose this stigma, the focus of the core WordPress team has clearly changed. With the [2016 State of the Word][5], by founder [Matthew Mullenweg][6], we can see a clear shift in goals, vision and effort. In 2016, we saw amazing progress when the WordPress core adopted a majority of the very popular [REST API plugin][7]. This was clearly an intention of the core team early on when they began a bold effort of building a front-end JavaScript CMS admin panel, that breaks away from the golden standard we have seen for so many years; they called it [Calpyso][8].


  [1]: https://wordpress.org/plugins/
  [2]: https://wordpress.org/themes/
  [3]: https://codex.wordpress.org/
  [4]: http://wordpress.stackexchange.com/questions/117387/wordpress-and-event-driven-programming-what-is-it-about
  [5]: https://ma.tt/2016/12/state-of-the-word-2016/
  [6]: https://en.wikipedia.org/wiki/Matt_Mullenweg
  [7]: https://developer.wordpress.org/rest-api/
  [8]: https://developer.wordpress.com/calypso/

## WordPress Themes
# Mapping URLs to specific templates
To fully grasp WordPress themes, you must understand two primary concepts:
1. Permalinks
2. The Template Hierarchy

A permalink is a permanent, non-changing URL (or link, to a specific resource. For instance: 
- example.com/about-us/ (a Page in WP)
- example.com/services/ (a listing of multiple items, also called an "archive" in WP lingo)
- example.com/services/we-can-do-that-for-you/ (an individual item)

When a user requests a URL, WordPress reverse-engineers the permalink to figure out which template should control its layout. WordPress looks for the various template files that *could* control this particular piece of content, and ultimately gives preference to the most specific one it finds. This is known as the Template Hierarchy.

Once WP finds the matching view template in the hierarchy, it uses that file to process and render the page.

For example:
`index.php` (the default,  "catch-all" template) will be overridden by `archive.php` (the default template for list-based content), which will in turn be overridden by `archive-services.php` (a template file specifically for the archive named "services").

[Here is a great visual reference for the Template Hierarchy][1]

# Basic Theme Directory Structure
A simple theme looks something like this:
      
        // Theme CSS
        style.css

        // Custom functionality for your theme
        functions.php

        // Partials to include in subsequent theme files
        header.php
        footer.php
        sidebar.php
        comments.php

        // "Archives", (listing views that contain multiple posts)
        archive.php
        author.php
        date.php
        taxonomy.php
        tag.php
        category.php

        // Individual content pages
        // Note that home and frontpage templates are not recommended
        // and they should be replaced by page templates
        singular.php
        single.php
        page.php
        front-page.php
        home.php

        // Misc. Utility Pages
        index.php (a catch-all if nothing else matches)
        search.php
        attachment.php
        image.php
        404.php

# Example of a "Single" (template for an individual post)

    <?php get_header(); ?>

    <?php if ( have_posts() ) while ( have_posts() ) : the_post(); ?>
        <h1><?php the_title(); ?></h1>
        <?php the_content(); ?>
        <?php comments_template( '', true ); ?>
    <?php endwhile; ?>

    <?php get_sidebar(); ?>
    <?php get_footer(); ?>
 
What's happening here? First, it loads `header.php` (similar to a PHP include or require), sets up The Loop, displays `the_title` and `the_content`, then includes `comments.php`, `sidebar.php`, and `footer.php`. The Loop does the heavy lifting, setting up a `Post` object, which contains all the information for the currently-viewed content.

# Example of an "Archive" (template for a list of multiple posts)    
    <?php get_header(); ?>

    <?php if ( have_posts() ) while ( have_posts() ) : the_post(); ?>
        <a href="<?php the_permalink(); ?>"<?php the_title(); ?></a>
        <?php the_excerpt(); ?>
    <?php endwhile; ?>

    <?php
        next_posts_link( 'Older Entries', $the_query->max_num_pages );
        previous_posts_link( 'Newer Entries' );
    ?>

    <?php get_sidebar(); ?>
    <?php get_footer(); ?>

First, it includes `header.php`, sets up The Loop, and includes `sidebar.php`, and `footer.php`. But in this case there are multiple posts in the loop, so instead an excerpt is shown with a link to the individual post. `next_posts_link` and `previous_posts_link` are also included so the archive can paginate results.

# Posts, Pages, Custom Post Types, and Custom Fields
Out of the box, WordPress supports two types of content: `Posts` and `Pages`. Posts are typically used for non-hierarchical content like blog posts. Pages are used for static, standalone content like an About Us page, or a company's Services page with nested sub-pages underneath.

As of version 3.0, developers can define their own custom post types to extend the functionality of WordPress beyond just the basics. In addition to custom post types, you can also create your own custom fields to attach to your posts/pages/custom post types, allowing you to provide a structured way of adding and accessing metadata within your templates. See: [Advanced Custom Fields][2].


  [1]: https://wphierarchy.com/
  [2]: https://www.advancedcustomfields.com/

